"""
Research metadata extraction and markdown note generation pipeline.

Hybrid processing approach:
1. Fast path: Regex-based classification for references with clear identifiers (DOI, arXiv, URL, ISBN)
2. LLM fallback: LLM classification for ambiguous references, then same deterministic processing

Both paths use a single fetch/generate/save pipeline, eliminating behavioral drift.
"""

import traceback
from typing import Dict, Any
from dotenv import load_dotenv

load_dotenv()  # Load env vars before importing modules that read them at import time

from research_extractor_tools import (
    classify_reference_fast,
    classify_reference_llm,
    process_reference_deterministic,
    parse_references_from_text,
    screen_file_content,
)
from research_extractor_constants import (
    HYBRID_MODE_ENABLED,
    CONFIDENCE_THRESHOLD,
    FAST_PATH_STATS,
)


def process_single_reference(
    reference: str,
    reference_num: int,
    total_refs: int,
    origin: str,
    output_dir: str,
    verbose: bool = False
) -> Dict[str, Any]:
    """
    Hybrid processing: deterministic for obvious cases, LLM classification for ambiguous.

    Uses a single processing pipeline: regex classification (fast path) or LLM
    classification (fallback), both routed through the same deterministic
    fetch/generate/save code.

    Args:
        reference: The reference text to process
        reference_num: Current reference number (for progress tracking)
        total_refs: Total number of references in batch
        origin: Source book/document name
        output_dir: Directory for saving markdown files
        verbose: If True, print detailed processing info

    Returns:
        Dictionary with processing result and status
    """
    print(f"\n{'='*80}")
    print(f"Processing reference {reference_num}/{total_refs}")
    print(f"{'='*80}")
    print(f"Reference: {reference[:200]}..." if len(reference) > 200 else f"Reference: {reference}")
    
    # Try fast path if hybrid mode enabled
    if HYBRID_MODE_ENABLED:
        fast_result = classify_reference_fast(reference)
        
        if fast_result["is_obvious"] and fast_result["confidence"] >= CONFIDENCE_THRESHOLD:
            # FAST PATH: Deterministic processing
            print(f"🚀 FAST PATH: {fast_result['reason']} (confidence: {fast_result['confidence']:.2f})")
            print(f"   Type: {fast_result['source_type']}, ID: {fast_result['identifier_type']}")
            
            det_result = process_reference_deterministic(
                reference, fast_result, origin, output_dir
            )
            
            if det_result is not None:
                # Fast path succeeded
                print(f"✅ SUCCESS (deterministic): {reference_num}/{total_refs}")
                det_result["path_used"] = "deterministic"
                det_result["confidence"] = fast_result["confidence"]
                det_result["reference"] = reference[:100]
                return det_result
            else:
                # Fast path failed - fallback to agent
                print(f"⚠️  Fast path failed, falling back to agent...")
        else:
            # Low confidence or ambiguous - use agent
            print(f"🤖 AGENT PATH: {fast_result['reason']}")
            if fast_result["confidence"] > 0:
                print(f"   Confidence too low ({fast_result['confidence']:.2f})")
    
    # LLM CLASSIFICATION FALLBACK: Use LLM to classify, then same deterministic processing
    print("🤖 Using LLM for classification...")

    try:
        llm_classification = classify_reference_llm(reference)

        if llm_classification is None:
            # LLM determined this is invalid/unclassifiable
            print(f"⏭️  SKIPPED (LLM classified as invalid): {reference_num}/{total_refs}")
            return {
                "status": "skipped",
                "reference": reference[:100],
                "reason": "LLM classified as invalid or unresolvable",
                "path_used": "agent",
                "confidence": 0.0
            }

        id_val = llm_classification.get('identifier_value') or ''
        print(f"🤖 LLM classified: {llm_classification['source_type']} "
              f"({llm_classification['identifier_type']}: {id_val[:50]})")

        # Route through the SAME deterministic processing as the fast path
        det_result = process_reference_deterministic(
            reference, llm_classification, origin, output_dir
        )

        if det_result is not None:
            print(f"✅ SUCCESS (LLM + deterministic): {reference_num}/{total_refs}")
            det_result["path_used"] = "agent"
            det_result["confidence"] = llm_classification["confidence"]
            det_result["reference"] = reference[:100]
            return det_result
        else:
            print(f"❌ FAILED (processing failed after LLM classification): {reference_num}/{total_refs}")
            return {
                "status": "failed",
                "reference": reference[:100],
                "reason": "Processing failed after LLM classification",
                "path_used": "agent",
                "confidence": llm_classification["confidence"]
            }

    except Exception as e:
        print(f"❌ Error processing reference: {str(e)}")
        if verbose:
            print(f"Traceback:\n{traceback.format_exc()}")
        return {
            "status": "failed",
            "reason": f"exception: {str(e)}",
            "reference": reference[:100],
            "path_used": "agent",
            "confidence": 0.0
        }


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
        "results": []
    }
    
    # Process each reference
    for i, ref in enumerate(refs, 1):
        result = process_single_reference(
            reference=ref,
            reference_num=i,
            total_refs=total_refs,
            origin=origin,
            output_dir=output_dir,
            verbose=verbose
        )
        
        # Track path usage
        if result.get("path_used") == "deterministic":
            stats["fast_path"] += 1
        else:
            stats["agent_path"] += 1
        
        # Track outcomes
        status = result.get("status", "unknown")
        if status == "success":
            stats["success"] += 1
        elif status == "skipped":
            stats["skipped"] += 1
        elif status == "uncertain":
            stats["uncertain"] += 1
        else:
            stats["failed"] += 1
        
        stats["results"].append(result)
    
    # Print final statistics
    print(f"\n{'='*80}")
    print(f"PROCESSING COMPLETE")
    print(f"{'='*80}")

    if stats['total'] == 0:
        print(f"\n📈 No valid references to process (all {stats['total_raw']} entries were pre-filtered).")
        print(f"{'='*80}\n")
        return stats

    if FAST_PATH_STATS:
        print(f"\n📊 HYBRID MODE STATISTICS:")
        print(f"  Fast Path (deterministic): {stats['fast_path']}/{stats['total']} " +
              f"({stats['fast_path']/stats['total']*100:.1f}%)")
        print(f"  Agent Path (LLM analysis): {stats['agent_path']}/{stats['total']} " +
              f"({stats['agent_path']/stats['total']*100:.1f}%)")
        print(f"\n📈 OUTCOME STATISTICS:")
    else:
        print(f"\n📈 STATISTICS:")
    
    print(f"  Total raw entries: {stats['total_raw']}")
    print(f"  Pre-filtered non-citations: {stats['prefiltered']}")
    if stats['split_compounds'] > 0:
        print(f"  Compound references split: {stats['split_compounds']}")
    print(f"  Valid references processed: {stats['total']}")
    print(f"  ✅ Success: {stats['success']}/{stats['total']} ({stats['success']/stats['total']*100:.1f}%)")
    print(f"  ⏭️  Skipped: {stats['skipped']}/{stats['total']} ({stats['skipped']/stats['total']*100:.1f}%)")
    print(f"  ❌ Failed: {stats['failed']}/{stats['total']} ({stats['failed']/stats['total']*100:.1f}%)")
    if stats['uncertain'] > 0:
        print(f"  ⚠️  Uncertain: {stats['uncertain']}/{stats['total']} ({stats['uncertain']/stats['total']*100:.1f}%)")
    
    # Print details of skipped/failed
    if stats['skipped'] > 0:
        print(f"\n📋 Skipped References:")
        for r in stats['results']:
            if r["status"] == "skipped":
                print(f"  - {r['reference']}... (Reason: {r.get('reason', 'unknown')})")
    
    if stats['failed'] > 0:
        print(f"\n🚨 Failed References:")
        for r in stats['results']:
            if r["status"] == "failed":
                print(f"  - {r['reference']}... (Reason: {r.get('reason', 'unknown')})")
    
    print(f"{'='*80}\n")
    
    return stats


if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(description="Extract research references into Obsidian markdown notes")
    parser.add_argument("input_file", help="Path to text file containing references")
    parser.add_argument("output_dir", help="Directory for saving markdown files")
    parser.add_argument("--origin", default="", help='Source book/document name (e.g., "[[Book Title]]")')
    parser.add_argument("--verbose", action="store_true", help="Print detailed agent reasoning")
    args = parser.parse_args()

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

# TODO - add a fallback that if it failed to find the reference based on it's source type, try websearch to learn more about it