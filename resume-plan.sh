#!/bin/bash
# Continue santa-lang Blitzen VM development with Claude Code

claude --dangerously-skip-permissions "Continue implementing santa-lang Blitzen VM. First, review git log and PLAN.md to find current progress. Read CLAUDE.md for development guidelines. Follow TDD - write tests first. Reference LANG.txt as source of truth. When a phase is complete: mark release gate items as [x] in PLAN.md, then commit with conventional commit (e.g. feat(lexer): implement tokens [Phase 1])."
