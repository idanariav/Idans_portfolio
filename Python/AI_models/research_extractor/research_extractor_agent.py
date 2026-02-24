"""
Research metadata extraction and markdown note generation pipeline.

Hybrid processing approach:
1. Fast path: Regex-based classification for references with clear identifiers (DOI, arXiv, URL, ISBN)
2. LLM fallback: LLM classification for ambiguous references, then same deterministic processing

Both paths use a single fetch/generate/save pipeline, eliminating behavioral drift.
"""

import re
import traceback
from typing import Dict, Any, Set
from dotenv import load_dotenv

load_dotenv()  # Load env vars before importing modules that read them at import time

from research_extractor_tools import (
    classify_reference_fast,
    classify_reference_llm,
    classify_references_batch_llm,
    extract_minimal_metadata_batch,
    extract_identifiers_from_text,
    process_reference_deterministic,
    parse_references_from_text,
    screen_file_content,
)
from research_extractor_constants import (
    HYBRID_MODE_ENABLED,
    CONFIDENCE_THRESHOLD,
    FAST_PATH_STATS,
    BATCH_SIZE,
    PATTERN_DOI,
    PATTERN_ARXIV,
)


def run_agent(
    input_file: str,
    output_dir: str,
    origin: str = "",
    verbose: bool = False
) -> Dict[str, Any]:
    """
    Run the research extractor agent on a batch of references.
    
    Uses hybrid mode: fast deterministic path for obvious references,
    agent fallback for ambiguous cases or failures.
    
    Processes all references from an input file, extracting metadata and
    generating structured markdown notes. Each reference is processed with
    a fresh agent context to prevent context window issues.
    
    Args:
        input_file: Path to text file containing references
        output_dir: Directory for saving markdown files
        origin: Source book/document name (e.g., "[[Book Title]]")
        verbose: If True, print detailed agent reasoning and tool calls
    
    Returns:
        Summary dictionary with:
        - total: Total references processed
        - success: Number of successfully saved notes
        - skipped: Number of skipped references
        - failed: Number of failed references
        - fast_path: Number using deterministic path
        - agent_path: Number using agent path
        - details: List of results for each reference
    
    Example:
        >>> result = run_agent(
        ...     input_file="/path/to/references.txt",
        ...     output_dir="/path/to/notes",
        ...     origin="[[My Book]]",
        ...     verbose=True
        ... )
        >>> print(f"Success: {result['success']}/{result['total']}")
    """
    print(f"\n{'#'*80}")
    print(f"# Research Extractor Agent (Hybrid Mode)")
    print(f"{'#'*80}")
    print(f"Input file: {input_file}")
    print(f"Output directory: {output_dir}")
    print(f"Origin: {origin}")
    print(f"Verbose mode: {verbose}")
    print(f"{'#'*80}\n")
    
    # Read and parse references using shared function
    try:
        with open(input_file, "r", encoding="utf-8") as f:
            text = f.read()

        # File-level screening: skip files with insufficient reference content
        should_skip, skip_reason = screen_file_content(text)
        if should_skip:
            print(f"⏭️  SKIPPED FILE: {skip_reason}")
            return {
                "total": 0, "total_raw": 0, "prefiltered": 0,
                "split_compounds": 0, "prefiltered_details": [],
                "split_details": [], "fast_path": 0, "agent_path": 0,
                "success": 0, "skipped": 0, "failed": 0, "uncertain": 0,
                "results": [], "file_skipped": True,
                "file_skip_reason": skip_reason,
            }

        parsed = parse_references_from_text(text)
        refs = parsed["references"]
        prefiltered = parsed["skipped"]
        split_info = parsed["split"]
        total_refs = parsed["valid_count"]
        total_raw = parsed["raw_count"]
        
        print(f"📚 Found {total_raw} raw entries in file")
        if prefiltered:
            print(f"🔍 Pre-filtered {len(prefiltered)} obvious non-citations:")
            for pf in prefiltered:
                print(f"   ⏭️  {pf['reference']}... ({pf['reason']})")
        if split_info:
            print(f"✂️  Split {len(split_info)} compound references:")
            for si in split_info:
                print(f"   📑 {si['original']}... → {si['count']} citations")
        print(f"📝 Processing {total_refs} valid references\n")
        
    except FileNotFoundError:
        return {
            "error": f"File not found: {input_file}",
            "total": 0,
            "success": 0,
            "skipped": 0,
            "failed": 0,
            "fast_path": 0,
            "agent_path": 0
        }
    except Exception as e:
        return {
            "error": f"Error reading file: {str(e)}",
            "total": 0,
            "success": 0,
            "skipped": 0,
            "failed": 0,
            "fast_path": 0,
            "agent_path": 0
        }
    
    # Statistics tracking
    stats = {
        "total": total_refs,
        "total_raw": total_raw,
        "prefiltered": len(prefiltered),
        "split_compounds": len(split_info),
        "prefiltered_details": prefiltered,
        "split_details": split_info,
        "fast_path": 0,
        "agent_path": 0,
        "success": 0,
        "skipped": 0,
        "failed": 0,
        "uncertain": 0,
        "Unresolvable": 0,
        "pre_extracted_total": 0,
        "pre_extracted_success": 0,
        "pre_extracted_failed": 0,
        "pre_extracted_skipped": 0,
        "results": []
    }
    
    # ================================================================
    # Phase 0: Document-level identifier pre-extraction
    # ================================================================
    identifiers = extract_identifiers_from_text(text)
    processed_ids: Set[str] = set()
    pre_extracted_refs: Set[int] = set()  # ref indices to skip in Phase 1-3

    if identifiers:
        stats["pre_extracted_total"] = len(identifiers)
        print(f"\n{'='*80}")
        print(f"PHASE 0: Document-Level Identifier Pre-Extraction")
        print(f"{'='*80}")
        print(f"Found {len(identifiers)} unique identifiers (DOI/arXiv)\n")

        for idx, ident in enumerate(identifiers):
            id_num = idx + 1
            id_type = ident["identifier_type"]
            id_value = ident["identifier_value"]

            classification = {
                "is_obvious": True,
                "confidence": 0.95,
                "source_type": ident["source_type"],
                "identifier_type": id_type,
                "identifier_value": id_value,
                "url": None,
                "reason": "Document-level pre-extraction",
            }

            try:
                result = process_reference_deterministic(
                    id_value, classification, origin, output_dir,
                )

                if result is not None:
                    processed_ids.add(id_value)
                    stats["pre_extracted_success"] += 1
                    if result.get("status") == "uncertain":
                        print(f"  ⚠️  [{id_num}/{len(identifiers)}] "
                              f"DEGRADED ({id_type}: {id_value[:50]})")
                    else:
                        print(f"  ✅ [{id_num}/{len(identifiers)}] "
                              f"{id_type}: {id_value[:50]}")
                    stats["results"].append(result)
                else:
                    stats["pre_extracted_failed"] += 1
                    print(f"  ❌ [{id_num}/{len(identifiers)}] "
                          f"FAILED ({id_type}: {id_value[:50]})")
                    stats["results"].append({
                        "status": "failed",
                        "reference": id_value,
                        "reason": "Pre-extraction processing failed",
                        "path_used": "pre-extraction",
                        "confidence": 0.95,
                    })
            except Exception as e:
                stats["pre_extracted_failed"] += 1
                print(f"  ❌ [{id_num}/{len(identifiers)}] ERROR: {str(e)}")
                stats["results"].append({
                    "status": "failed",
                    "reference": id_value,
                    "reason": f"Pre-extraction exception: {str(e)}",
                    "path_used": "pre-extraction",
                    "confidence": 0.95,
                })

        print(f"\n📊 Pre-extracted: {stats['pre_extracted_success']}/{len(identifiers)} "
              f"success, {stats['pre_extracted_failed']}/{len(identifiers)} failed")
    else:
        print(f"\n📋 No document-level identifiers found (DOI/arXiv)")

    # ================================================================
    # Phase 1: Fast path classification
    # ================================================================
    print(f"\n{'='*80}")
    print(f"PHASE 1: Fast Path Classification")
    print(f"{'='*80}\n")

    fast_results = {}  # {index: classification_dict}
    needs_llm = []     # [(index, ref_text)]

    for i, ref in enumerate(refs):
        # Dedup: skip references whose identifier was already pre-extracted
        if processed_ids:
            doi_match = re.search(PATTERN_DOI, ref)
            arxiv_match = re.search(PATTERN_ARXIV, ref, re.IGNORECASE)
            matched_id = None
            if doi_match and doi_match.group(1) in processed_ids:
                matched_id = doi_match.group(1)
            elif arxiv_match and f"arXiv:{arxiv_match.group(1)}" in processed_ids:
                matched_id = f"arXiv:{arxiv_match.group(1)}"
            if matched_id:
                pre_extracted_refs.add(i)
                stats["pre_extracted_skipped"] += 1
                print(f"  ⏭️  [{i+1}/{total_refs}] Already pre-extracted "
                      f"({matched_id[:50]})")
                continue

        if HYBRID_MODE_ENABLED:
            fast_result = classify_reference_fast(ref)

            if fast_result["is_obvious"] and fast_result["confidence"] >= CONFIDENCE_THRESHOLD:
                fast_results[i] = fast_result
                print(f"  🚀 [{i+1}/{total_refs}] Fast path: {fast_result['source_type']} "
                      f"({fast_result['confidence']:.2f})")
            else:
                needs_llm.append((i, ref))
                print(f"  ⏸  [{i+1}/{total_refs}] Needs LLM: {fast_result['reason']}")
        else:
            needs_llm.append((i, ref))

    print(f"\n📊 Fast path: {len(fast_results)}/{total_refs}, "
          f"Need LLM: {len(needs_llm)}/{total_refs}, "
          f"Pre-extracted: {len(pre_extracted_refs)}/{total_refs}")

    # ================================================================
    # Phase 2: Batch LLM classification
    # ================================================================
    llm_results = {}  # {index: classification_dict_or_None}

    if needs_llm:
        print(f"\n{'='*80}")
        print(f"PHASE 2: Batch LLM Classification (batch size: {BATCH_SIZE})")
        print(f"{'='*80}\n")

        total_batches = (len(needs_llm) + BATCH_SIZE - 1) // BATCH_SIZE

        for batch_start in range(0, len(needs_llm), BATCH_SIZE):
            batch_items = needs_llm[batch_start:batch_start + BATCH_SIZE]
            batch_indices = [idx for idx, _ in batch_items]
            batch_refs = [ref for _, ref in batch_items]
            batch_num = (batch_start // BATCH_SIZE) + 1

            print(f"  🤖 Batch {batch_num}/{total_batches} "
                  f"({len(batch_refs)} references)...")

            batch_classifications = classify_references_batch_llm(batch_refs)

            for local_idx, global_idx in enumerate(batch_indices):
                ref = batch_refs[local_idx]
                classification = batch_classifications.get(local_idx)

                if classification is not None:
                    llm_results[global_idx] = classification
                    print(f"    ✓ [{global_idx+1}] {classification['source_type']}")
                else:
                    # Fallback to single LLM call
                    print(f"    ⚠️  [{global_idx+1}] Batch miss, single LLM fallback...")
                    fallback = classify_reference_llm(ref)
                    llm_results[global_idx] = fallback  # may be None (invalid)
                    if fallback is not None:
                        print(f"    ✓ [{global_idx+1}] {fallback['source_type']} (fallback)")
                    else:
                        print(f"    ⏭️  [{global_idx+1}] Invalid (LLM)")

        classified = sum(1 for v in llm_results.values() if v is not None)
        print(f"\n📊 Batch LLM: {classified}/{len(needs_llm)} classified, "
              f"{len(needs_llm) - classified} invalid")

    # ================================================================
    # Phase 2.5: Batch minimal metadata for Unresolvable/Other
    # ================================================================
    # Collect references that are already known to need minimal metadata
    # extraction (classified as Unresolvable or Other). These always call
    # _extract_minimal_metadata in process_reference_deterministic, so
    # batching them here avoids N individual LLM calls.
    pre_extracted = {}  # {global_index: metadata_dict}
    unresolvable_indices: Set[int] = set()  # Track indices with Unresolvable classification

    unresolvable_refs = []  # [(global_index, ref_text)]
    all_classifications = {**fast_results, **llm_results}
    for i, ref in enumerate(refs):
        cls = all_classifications.get(i)
        if cls and cls.get("source_type") in ("Unresolvable", "Other"):
            unresolvable_refs.append((i, ref))
            unresolvable_indices.add(i)

    if unresolvable_refs:
        print(f"\n{'='*80}")
        print(f"PHASE 2.5: Batch Minimal Metadata Extraction "
              f"({len(unresolvable_refs)} references)")
        print(f"{'='*80}\n")

        for batch_start in range(0, len(unresolvable_refs), BATCH_SIZE):
            batch_items = unresolvable_refs[batch_start:batch_start + BATCH_SIZE]
            batch_indices = [idx for idx, _ in batch_items]
            batch_citations = [ref for _, ref in batch_items]

            batch_extractions = extract_minimal_metadata_batch(batch_citations)

            for local_idx, global_idx in enumerate(batch_indices):
                extraction = batch_extractions.get(local_idx)
                if extraction is not None:
                    pre_extracted[global_idx] = extraction
                    title = extraction.get("title", "?")[:50]
                    print(f"  ✓ [{global_idx+1}] {title}")
                else:
                    print(f"  ⚠️  [{global_idx+1}] Batch miss (will fallback)")

        extracted_count = len(pre_extracted)
        print(f"\n📊 Pre-extracted: {extracted_count}/{len(unresolvable_refs)}")

    # ================================================================
    # Phase 3: Deterministic processing
    # ================================================================
    print(f"\n{'='*80}")
    print(f"PHASE 3: Deterministic Processing")
    print(f"{'='*80}")

    for i, ref in enumerate(refs):
        # Skip references already handled by Phase 0 pre-extraction
        if i in pre_extracted_refs:
            continue

        ref_num = i + 1
        print(f"\n{'='*80}")
        print(f"Processing reference {ref_num}/{total_refs}")
        print(f"{'='*80}")
        print(f"Reference: {ref[:200]}..." if len(ref) > 200 else f"Reference: {ref}")

        if i in fast_results:
            classification = fast_results[i]
            path_used = "deterministic"
            print(f"🚀 FAST PATH: {classification['reason']} "
                  f"(confidence: {classification['confidence']:.2f})")
            print(f"   Type: {classification['source_type']}, "
                  f"ID: {classification['identifier_type']}")
        else:
            classification = llm_results.get(i)
            path_used = "agent"

            if classification is None:
                print(f"⏭️  SKIPPED (LLM classified as invalid): {ref_num}/{total_refs}")
                stats["agent_path"] += 1
                stats["skipped"] += 1
                stats["results"].append({
                    "status": "skipped",
                    "reference": ref[:100],
                    "reason": "LLM classified as invalid or unresolvable",
                    "path_used": "agent",
                    "confidence": 0.0,
                })
                continue

            id_val = classification.get("identifier_value") or ""
            print(f"🤖 LLM classified: {classification['source_type']} "
                  f"({classification['identifier_type']}: {id_val[:50]})")

        try:
            det_result = process_reference_deterministic(
                ref, classification, origin, output_dir,
                pre_extracted_metadata=pre_extracted.get(i),
            )

            if det_result is not None:
                det_result["path_used"] = path_used
                det_result["confidence"] = classification["confidence"]
                det_result["reference"] = ref[:100]
                if det_result.get("status") == "uncertain":
                    orig = det_result.get("original_source_type", "?")
                    reason = det_result.get("note", "")
                    print(f"⚠️  DEGRADED ({orig} → Unresolvable, {path_used}): "
                          f"{ref_num}/{total_refs} — {reason}")
                else:
                    print(f"✅ SUCCESS ({path_used}): {ref_num}/{total_refs}")
                stats["success"] += 1
            else:
                print(f"❌ FAILED (processing failed): {ref_num}/{total_refs}")
                det_result = {
                    "status": "failed",
                    "reference": ref[:100],
                    "reason": "Processing failed after classification",
                    "path_used": path_used,
                    "confidence": classification["confidence"],
                }
                stats["failed"] += 1

        except Exception as e:
            print(f"❌ Error processing reference: {str(e)}")
            if verbose:
                print(f"Traceback:\n{traceback.format_exc()}")
            det_result = {
                "status": "failed",
                "reason": f"exception: {str(e)}",
                "reference": ref[:100],
                "path_used": path_used,
                "confidence": classification["confidence"],
            }
            stats["failed"] += 1

        if path_used == "deterministic":
            stats["fast_path"] += 1
        else:
            stats["agent_path"] += 1

        # Count Unresolvable references separately
        if i in unresolvable_indices:
            stats["Unresolvable"] += 1
        else:
            status = det_result.get("status", "unknown")
            if status == "uncertain":
                stats["uncertain"] += 1

        stats["results"].append(det_result)
    
    # Print final statistics
    print(f"\n{'='*80}")
    print(f"PROCESSING COMPLETE")
    print(f"{'='*80}")

    # Pre-extraction summary
    pre_total = stats['pre_extracted_total']
    if pre_total > 0:
        print(f"\n📋 PHASE 0 — DOCUMENT-LEVEL PRE-EXTRACTION:")
        print(f"  Unique identifiers found: {pre_total}")
        print(f"  ✅ Success: {stats['pre_extracted_success']}/{pre_total}")
        print(f"  ❌ Failed: {stats['pre_extracted_failed']}/{pre_total}")
        print(f"  References skipped (dedup): {stats['pre_extracted_skipped']}")

    # Reference pipeline summary
    ref_processed = stats['total'] - stats['pre_extracted_skipped']

    if stats['total'] == 0 and pre_total == 0:
        print(f"\n📈 No valid references to process (all {stats['total_raw']} entries were pre-filtered).")
        print(f"{'='*80}\n")
        return stats

    if ref_processed > 0:
        if FAST_PATH_STATS:
            print(f"\n📊 HYBRID MODE STATISTICS (reference pipeline):")
            print(f"  Fast Path (deterministic): {stats['fast_path']}/{ref_processed} " +
                  f"({stats['fast_path']/ref_processed*100:.1f}%)")
            print(f"  Agent Path (LLM analysis): {stats['agent_path']}/{ref_processed} " +
                  f"({stats['agent_path']/ref_processed*100:.1f}%)")

        print(f"\n📈 REFERENCE PIPELINE STATISTICS:")
        print(f"  Total raw entries: {stats['total_raw']}")
        print(f"  Pre-filtered non-citations: {stats['prefiltered']}")
        if stats['split_compounds'] > 0:
            print(f"  Compound references split: {stats['split_compounds']}")
        print(f"  Valid references processed: {ref_processed}"
              f" (of {stats['total']} total, {stats['pre_extracted_skipped']} skipped via pre-extraction)")
        print(f"  ✅ Success: {stats['success']}/{ref_processed} ({stats['success']/ref_processed*100:.1f}%)")
        print(f"  ⏭️  Skipped: {stats['skipped']}/{ref_processed} ({stats['skipped']/ref_processed*100:.1f}%)")
        print(f"  ❌ Failed: {stats['failed']}/{ref_processed} ({stats['failed']/ref_processed*100:.1f}%)")
        if stats['uncertain'] > 0:
            print(f"  ⚠️  Uncertain: {stats['uncertain']}/{ref_processed} ({stats['uncertain']/ref_processed*100:.1f}%)")
        if stats['Unresolvable'] > 0:
            print(f"  📄 Unresolvable: {stats['Unresolvable']}/{ref_processed} ({stats['Unresolvable']/ref_processed*100:.1f}%)")

    # Print details of skipped/failed
    if stats['skipped'] > 0:
        print(f"\n📋 Skipped References:")
        for r in stats['results']:
            if r["status"] == "skipped":
                print(f"  - {r['reference']}... (Reason: {r.get('reason', 'unknown')})")

    failed_count = stats['failed'] + stats['pre_extracted_failed']
    if failed_count > 0:
        print(f"\n🚨 Failed References:")
        for r in stats['results']:
            if r["status"] == "failed":
                print(f"  - {r['reference'][:80]}... (Reason: {r.get('reason', 'unknown')})")

    print(f"{'='*80}\n")
    
    return stats


def run_agent_from_mapping(
    mapping_file: str,
    output_dir: str,
    verbose: bool = False,
) -> Dict[str, Any]:
    """
    Run the research extractor agent on multiple files using a mapping file.

    The mapping file is a JSON file where each key is a filename (relative to
    the mapping file's directory) and each value is the origin string
    (e.g., "[[Atomic Habits (book)]]").

    Args:
        mapping_file: Path to JSON mapping file
        output_dir: Directory for saving markdown files
        verbose: If True, print detailed agent reasoning

    Returns:
        Aggregate summary with per-file results
    """
    import json
    from pathlib import Path

    mapping_path = Path(mapping_file)
    mapping_dir = mapping_path.parent

    with open(mapping_path, "r", encoding="utf-8") as f:
        mapping = json.load(f)

    print(f"\n{'#'*80}")
    print(f"# Batch Processing from Mapping File")
    print(f"# {len(mapping)} files to process")
    print(f"{'#'*80}\n")

    aggregate = {
        "total_files": len(mapping),
        "files_success": 0,
        "files_failed": 0,
        "total_refs": 0,
        "total_success": 0,
        "total_skipped": 0,
        "total_failed": 0,
        "per_file": [],
    }

    for i, (filename, origin) in enumerate(mapping.items(), 1):
        input_path = mapping_dir / filename
        print(f"\n{'*'*80}")
        print(f"* File {i}/{len(mapping)}: {filename}")
        print(f"* Origin: {origin}")
        print(f"{'*'*80}")

        if not input_path.exists():
            print(f"  ⚠️  File not found: {input_path}")
            aggregate["files_failed"] += 1
            aggregate["per_file"].append({
                "file": filename,
                "origin": origin,
                "error": "File not found",
            })
            continue

        result = run_agent(
            input_file=str(input_path),
            output_dir=output_dir,
            origin=origin,
            verbose=verbose,
        )

        aggregate["files_success"] += 1
        aggregate["total_refs"] += result.get("total", 0)
        aggregate["total_success"] += result.get("success", 0)
        aggregate["total_skipped"] += result.get("skipped", 0)
        aggregate["total_failed"] += result.get("failed", 0)
        aggregate["per_file"].append({
            "file": filename,
            "origin": origin,
            "result": result,
        })

    print(f"\n{'#'*80}")
    print(f"# BATCH PROCESSING COMPLETE")
    print(f"{'#'*80}")
    print(f"  Files processed: {aggregate['files_success']}/{aggregate['total_files']}")
    if aggregate["files_failed"] > 0:
        print(f"  Files not found: {aggregate['files_failed']}")
    print(f"  Total references: {aggregate['total_refs']}")
    print(f"  ✅ Success: {aggregate['total_success']}")
    print(f"  ⏭️  Skipped: {aggregate['total_skipped']}")
    print(f"  ❌ Failed: {aggregate['total_failed']}")
    print(f"{'#'*80}\n")

    return aggregate


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Extract research references into Obsidian markdown notes")
    parser.add_argument("input_file", nargs="?", help="Path to text file containing references")
    parser.add_argument("output_dir", help="Directory for saving markdown files")
    parser.add_argument("--origin", default="", help='Source book/document name (e.g., "[[Book Title]]")')
    parser.add_argument("--mapping-file", help="Path to JSON mapping file (filename -> origin)")
    parser.add_argument("--verbose", action="store_true", help="Print detailed agent reasoning")
    args = parser.parse_args()

    if args.mapping_file:
        result = run_agent_from_mapping(
            mapping_file=args.mapping_file,
            output_dir=args.output_dir,
            verbose=args.verbose,
        )

        print(f"\n✨ Final Summary:")
        print(f"   Files: {result['files_success']}/{result['total_files']}")
        if result['total_refs'] > 0:
            print(f"   Success rate: {result['total_success']}/{result['total_refs']} "
                  f"({result['total_success']/result['total_refs']*100:.1f}%)")
        else:
            print(f"   No references found or processed")
    elif args.input_file:
        result = run_agent(
            input_file=args.input_file,
            output_dir=args.output_dir,
            origin=args.origin,
            verbose=args.verbose,
        )

        print(f"\n✨ Final Summary:")
        if result['total'] > 0:
            print(f"   Success rate: {result['success']}/{result['total']} "
                  f"({result['success']/result['total']*100:.1f}%)")
        else:
            print(f"   No references found or processed")
    else:
        parser.error("Either input_file or --mapping-file is required")

# TODO - add a fallback that if it failed to find the reference based on it's source type, try websearch to learn more about it