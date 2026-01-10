"""
LangChain function-calling agent for research metadata extraction and markdown note generation.

Optimized workflow reduces LLM calls by:
1. Merging classification + extraction into single analyze_reference tool
2. Inlining content preparation into fetch tools
3. Using function calling instead of ReAct for reduced reasoning overhead

Each reference is processed with a fresh agent context to prevent context window issues.
"""

import os
import traceback
from typing import Dict, Any
from dotenv import load_dotenv

from langchain_openai import ChatOpenAI
from langchain.agents import create_tool_calling_agent, AgentExecutor
from langchain_core.prompts import ChatPromptTemplate, MessagesPlaceholder

from research_extractor_tools import (
    TOOLS,
    classify_reference_fast,
    process_reference_deterministic,
    parse_references_from_text,
)
from research_extractor_prompts import AGENT_SYSTEM_PROMPT
from research_extractor_constants import (
    MODEL,
    OPENROUTER_API_BASE,
    HYBRID_MODE_ENABLED,
    CONFIDENCE_THRESHOLD,
    FAST_PATH_STATS,
)

load_dotenv()


def create_agent_executor(verbose: bool = False):
    """
    Create a LangChain function-calling agent with research extraction tools.
    
    Uses function calling instead of ReAct to reduce reasoning overhead and
    improve execution speed.
    
    Args:
        verbose: If True, print agent reasoning and tool calls
    
    Returns:
        Configured AgentExecutor
    """
    # Initialize LLM with OpenRouter configuration
    llm = ChatOpenAI(
        model=MODEL,
        openai_api_key=os.getenv("OPENROUTER_API_KEY"),
        openai_api_base=OPENROUTER_API_BASE,
        temperature=0.0,  # Deterministic for consistency
    )
    
    # Create function-calling agent (more efficient than ReAct)
    prompt = ChatPromptTemplate.from_messages([
        ("system", AGENT_SYSTEM_PROMPT),
        ("human", "{input}"),
        MessagesPlaceholder(variable_name="agent_scratchpad"),
    ])
    
    agent = create_tool_calling_agent(llm, TOOLS, prompt)
    
    # Create agent executor with optimized settings
    agent_executor = AgentExecutor(
        agent=agent,
        tools=TOOLS,
        verbose=verbose,
        handle_parsing_errors=True,
        max_iterations=15,  # Prevent runaway execution
        early_stopping_method="force",
    )
    
    return agent_executor


def process_single_reference(
    reference: str,
    reference_num: int,
    total_refs: int,
    origin: str,
    output_dir: str,
    verbose: bool = False
) -> Dict[str, Any]:
    """
    Hybrid processing: deterministic for obvious cases, agent for ambiguous.
    
    Args:
        reference: The reference text to process
        reference_num: Current reference number (for progress tracking)
        total_refs: Total number of references in batch
        origin: Source book/document name
        output_dir: Directory for saving markdown files
        verbose: If True, print agent reasoning
    
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
    
    # AGENT FALLBACK: Use agent for ambiguous cases or fast path failures
    print("🤖 Using agent for full analysis...")
    
    # Create fresh agent for this reference (context reset)
    agent = create_agent_executor(verbose=verbose)
    
    # Prepare agent instruction with optimized workflow
    instruction = f"""Process this single reference and save it as a markdown note:

Reference: {reference}

Origin: {origin}
Output Directory: {output_dir}

WORKFLOW:
1. Analyze reference (use analyze_reference - returns source_type, identifier, AND validation in one call)
2. IF not valid: SKIP with validation_reason
3. IF Book:
   - Fetch book metadata
   - Save to reading list
4. ELSE (Research Paper/Article/etc):
   - Fetch metadata (fetch_paper_metadata for Research Paper, fetch_web_content for others)
   - Generate note (use content_for_note field from metadata)
   - Save markdown

IMPORTANT: You have full authority to determine if this reference should be skipped.
Skip if: identifier is meaningless (Ibid, loc.cit, etc), too vague, or no actionable information.

Report final status: SUCCESS, SKIPPED (reason), or FAILED (reason).
"""
    
    try:
        # Execute agent with optimized function calling
        if verbose:
            print("\n🤖 Agent starting analysis...")
        
        result = agent.invoke({"input": instruction})
        
        # Extract output and determine status
        output = result.get("output", "")
        
        # Check for success indicators in output
        if "SUCCESS" in output.upper():
            print(f"✅ SUCCESS (agent): {reference_num}/{total_refs}")
            return {
                "status": "success",
                "reference": reference[:100],
                "output": output,
                "path_used": "agent",
                "confidence": 0.0
            }
        elif "SKIP" in output.upper():
            print(f"⏭️  SKIPPED (agent): {reference_num}/{total_refs}")
            return {
                "status": "skipped",
                "reference": reference[:100],
                "reason": output,
                "path_used": "agent",
                "confidence": 0.0
            }
        else:
            print(f"⚠️  UNCERTAIN (agent): {reference_num}/{total_refs}")
            return {
                "status": "uncertain",
                "reference": reference[:100],
                "output": output,
                "path_used": "agent",
                "confidence": 0.0
            }
        
    except Exception as e:
        print(f"❌ Error processing reference: {str(e)}")
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
    
    # Process each reference with fresh context
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
    # Example usage
    result = run_agent(
        input_file="/Users/idanariav/Downloads/tecnhnical_inbox/a_significant_life.txt",
        output_dir="/Users/idanariav/Downloads/tecnhnical_inbox",
        origin="[[A Significant Life (book)]]",
        verbose=True  # Set to False to hide agent reasoning
    )
    
    print(f"\n✨ Final Summary:")
    if result['total'] > 0:
        print(f"   Success rate: {result['success']}/{result['total']} "
              f"({result['success']/result['total']*100:.1f}%)")
    else:
        print(f"   No references found or processed")

# TODO - add a fallback that if it failed to find the reference based on it's source type, try websearch to learn more about it