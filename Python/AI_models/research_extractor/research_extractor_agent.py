"""
LangChain ReAct agent for research metadata extraction and markdown note generation.

This agent processes batches of references, extracting metadata and generating
structured markdown notes optimized for Obsidian. Each reference is processed
with a fresh agent context to prevent context window issues.
"""

import os
import re
from typing import Dict, Any
from dotenv import load_dotenv

from langchain_openai import ChatOpenAI
from langchain.agents import create_agent

from research_extractor_tools import TOOLS
from research_extractor_prompts import AGENT_SYSTEM_PROMPT
from research_extractor_constants import MODEL, OPENROUTER_API_BASE

load_dotenv()


def create_agent_executor(verbose: bool = False):
    """
    Create a LangChain agent with research extraction tools.
    
    Args:
        verbose: If True, print agent reasoning and tool calls
    
    Returns:
        Configured CompiledStateGraph (agent)
    """
    # Initialize LLM with OpenRouter configuration
    llm = ChatOpenAI(
        model=MODEL,
        openai_api_key=os.getenv("OPENROUTER_API_KEY"),
        openai_api_base=OPENROUTER_API_BASE,
        temperature=0.0,  # Deterministic for consistency
    )
    
    # Create agent with new LangChain 1.x API
    # Always set debug=False to avoid verbose internal state dumps
    agent = create_agent(
        model=llm,
        tools=TOOLS,
        system_prompt=AGENT_SYSTEM_PROMPT,
        debug=False,
    )
    
    return agent


def process_single_reference(
    reference: str,
    reference_num: int,
    total_refs: int,
    origin: str,
    output_dir: str,
    verbose: bool = False
) -> Dict[str, Any]:
    """
    Process a single reference with a fresh agent context.
    
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
    
    # Create fresh agent for this reference (context reset)
    agent = create_agent_executor(verbose=verbose)
    
    # Prepare agent instruction
    instruction = f"""Process this single reference and save it as a markdown note:

Reference: {reference}

Origin: {origin}
Output Directory: {output_dir}

Follow the workflow:
1. Classify source type
2. Extract identifier
3. Validate identifier (skip if invalid)
4. IF Book:
   - Fetch book metadata (use fetch_book_metadata)
   - Save to reading list (use save_book_to_reading_list with origin and output_dir)
5. ELSE (Research Paper/Article/etc):
   - Fetch metadata (use fetch_paper_metadata for Research Paper, fetch_web_content for others)
   - Prepare content (use prepare_content_for_note to extract text from metadata)
   - Generate note (use prepared content string)
   - Save markdown

Report the final status: SUCCESS, SKIPPED (with reason), or FAILED (with reason).
"""
    
    try:
        # Track tool results for status determination
        tool_results = {}
        
        def parse_tool_result(msg):
            """Extract and store tool results from message."""
            if not hasattr(msg, "content") or not msg.content:
                return
            
            try:
                import json
                content_str = str(msg.content)
                result = json.loads(content_str) if isinstance(content_str, str) else content_str
                
                if isinstance(result, dict):
                    # Store specific fields we care about
                    if "source_type" in result:
                        tool_results["source_type"] = result["source_type"]
                    if "is_valid" in result:
                        tool_results["is_valid"] = result["is_valid"]
                    if "file_path" in result:
                        tool_results["file_saved"] = True
            except:
                pass
        
        if verbose:
            # Stream events for human-readable progress
            print("\n🤖 Agent starting workflow...")
            result = None
            for event in agent.stream({"messages": [{"role": "user", "content": instruction}]}):
                # Print tool calls in a clean format
                if "model" in event:
                    messages = event["model"].get("messages", [])
                    for msg in messages:
                        if hasattr(msg, "tool_calls") and msg.tool_calls:
                            for tool_call in msg.tool_calls:
                                tool_name = tool_call.get("name", "unknown")
                                print(f"  🔧 Calling tool: {tool_name}")
                
                # Print tool results and capture key values
                if "tools" in event:
                    tool_messages = event["tools"].get("messages", [])
                    for msg in tool_messages:
                        if hasattr(msg, "content"):
                            content_str = str(msg.content)
                            print(f"  ✓ Tool result: {content_str[:150]}...")
                            parse_tool_result(msg)
                
                # Keep the last event as result
                result = event
        else:
            # Non-verbose mode - just invoke
            result = agent.invoke({"messages": [{"role": "user", "content": instruction}]})
            
            # Extract tool results from message history
            messages = result.get("messages", [])
            for msg in messages:
                parse_tool_result(msg)
        
        # Determine status from tool results only
        source_type = tool_results.get("source_type")
        is_valid = tool_results.get("is_valid")
        file_saved = tool_results.get("file_saved", False)
        
        if verbose:
            print(f"\n🔍 Debug - file_saved: {file_saved}, source_type: {source_type}, is_valid: {is_valid}")
        
        # Status determination based solely on tool results
        if is_valid is False:
            status = "skipped"
            reason = "invalid identifier"
        elif file_saved:
            # save_markdown or save_book_to_reading_list was called successfully - this is success
            status = "success"
            reason = None
        else:
            # No file saved and not skipped - something went wrong
            status = "failed"
            reason = "workflow incomplete"
        
        # Print parsed status
        status_emoji = "✅" if status == "success" else "⏭️" if status == "skipped" else "❌"
        reason_text = f" - {reason}" if reason else ""
        print(f"\n{status_emoji} Status: {status.upper()}{reason_text}")
        
        return {
            "status": status,
            "reason": reason,
            "reference": reference[:100]
        }
        
    except Exception as e:
        import traceback
        print(f"❌ Error processing reference: {str(e)}")
        print(f"Traceback:\n{traceback.format_exc()}")
        return {
            "status": "failed",
            "reason": f"exception: {str(e)}",
            "reference": reference[:100]
        }


def run_agent(
    input_file: str,
    output_dir: str,
    origin: str = "",
    verbose: bool = False
) -> Dict[str, Any]:
    """
    Run the research extractor agent on a batch of references.
    
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
    print(f"# Research Extractor Agent")
    print(f"{'#'*80}")
    print(f"Input file: {input_file}")
    print(f"Output directory: {output_dir}")
    print(f"Origin: {origin}")
    print(f"Verbose mode: {verbose}")
    print(f"{'#'*80}\n")
    
    # Read and parse references
    try:
        with open(input_file, "r", encoding="utf-8") as f:
            text = f.read()
        
        # Use same parsing logic as tool
        refs = [r.strip() for r in re.split(r"\n\s*\n", text) if r.strip()]
        if len(refs) <= 1:
            refs = [r.strip() for r in re.split(r"\.\s+\d{1,2}\.\s+", text) if r.strip()]
        
        total_refs = len(refs)
        print(f"📚 Found {total_refs} references to process\n")
        
    except FileNotFoundError:
        return {
            "error": f"File not found: {input_file}",
            "total": 0,
            "success": 0,
            "skipped": 0,
            "failed": 0
        }
    except Exception as e:
        return {
            "error": f"Error reading file: {str(e)}",
            "total": 0,
            "success": 0,
            "skipped": 0,
            "failed": 0
        }
    
    # Process each reference with fresh context
    results = []
    for i, ref in enumerate(refs, 1):
        result = process_single_reference(
            reference=ref,
            reference_num=i,
            total_refs=total_refs,
            origin=origin,
            output_dir=output_dir,
            verbose=verbose
        )
        results.append(result)
    
    # Compile summary
    success_count = sum(1 for r in results if r["status"] == "success")
    skipped_count = sum(1 for r in results if r["status"] == "skipped")
    failed_count = sum(1 for r in results if r["status"] == "failed")
    
    # Print summary
    print(f"\n{'='*80}")
    print(f"PROCESSING COMPLETE")
    print(f"{'='*80}")
    print(f"Total references: {total_refs}")
    print(f"✅ Successfully saved: {success_count}")
    print(f"⏭️  Skipped: {skipped_count}")
    print(f"❌ Failed: {failed_count}")
    
    # Print details of skipped/failed
    if skipped_count > 0:
        print(f"\n📋 Skipped References:")
        for r in results:
            if r["status"] == "skipped":
                print(f"  - {r['reference']}... (Reason: {r.get('reason', 'unknown')})")
    
    if failed_count > 0:
        print(f"\n🚨 Failed References:")
        for r in results:
            if r["status"] == "failed":
                print(f"  - {r['reference']}... (Reason: {r.get('reason', 'unknown')})")
    
    print(f"{'='*80}\n")
    
    return {
        "total": total_refs,
        "success": success_count,
        "skipped": skipped_count,
        "failed": failed_count,
        "details": results
    }


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
