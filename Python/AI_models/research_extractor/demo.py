"""
Quick start example for the Research Extractor Agent.

This script demonstrates basic usage with sample references.
"""

import os
import tempfile
from research_extractor_agent import run_agent


def quick_demo():
    """Run a quick demo with sample references."""
    
    # Sample references to process
    sample_references = """
Attention Is All You Need
Vaswani, Ashish, et al.
Neural Information Processing Systems, 2017
DOI: 10.48550/arXiv.1706.03762

https://www.nytimes.com/2023/05/15/technology/ai-language-models.html

The human brain is a prediction machine: The German physician Hermann von Helmholtz developed the idea of the brain being a "prediction machine." This concept has become central to modern neuroscience and cognitive science.
"""
    
    # Create temporary input file
    with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False, encoding='utf-8') as f:
        f.write(sample_references)
        input_file = f.name
    
    # Create temporary output directory
    output_dir = tempfile.mkdtemp()
    
    print("\n" + "="*80)
    print("RESEARCH EXTRACTOR AGENT - QUICK DEMO")
    print("="*80)
    print(f"\nInput file: {input_file}")
    print(f"Output directory: {output_dir}")
    print("\nSample references:")
    print("-" * 80)
    print(sample_references)
    print("-" * 80)
    
    try:
        # Run the agent
        result = run_agent(
            input_file=input_file,
            output_dir=output_dir,
            origin="[[Demo]]",
            verbose=True  # Show agent reasoning
        )
        
        # Show results
        print("\n" + "="*80)
        print("DEMO COMPLETE!")
        print("="*80)
        print(f"\nCheck the output directory for generated markdown files:")
        print(f"  {output_dir}")
        print(f"\nResults:")
        print(f"  - Total: {result['total']}")
        print(f"  - Success: {result['success']}")
        print(f"  - Skipped: {result['skipped']}")
        print(f"  - Failed: {result['failed']}")
        
        # List created files
        print(f"\n📁 Created files:")
        for root, dirs, files in os.walk(output_dir):
            for file in files:
                if file.endswith('.md'):
                    rel_path = os.path.relpath(os.path.join(root, file), output_dir)
                    print(f"  ✅ {rel_path}")
        
        print("\n" + "="*80)
        
    finally:
        # Cleanup (optional - comment out to keep files for inspection)
        # os.unlink(input_file)
        # import shutil
        # shutil.rmtree(output_dir, ignore_errors=True)
        pass


if __name__ == "__main__":
    print("\n⚠️  Note: This demo requires:")
    print("  1. LangChain packages installed: pip install -r requirements.txt")
    print("  2. OpenRouter API key in .env file")
    print("  3. Optional: Tavily and NYT API keys for web content\n")
    
    response = input("Continue with demo? (y/n): ")
    if response.lower() == 'y':
        quick_demo()
    else:
        print("Demo cancelled.")
