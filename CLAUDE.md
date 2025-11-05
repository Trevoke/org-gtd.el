# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

**Note**: This project uses [bd (beads)](https://github.com/steveyegge/beads) for issue tracking. Use `bd` commands instead of markdown TODOs. See AGENTS.md for workflow details.

## Development Commands

### Setup Requirements

**Install Eldev** (if not already installed):
```bash
curl -fsSL https://raw.github.com/doublep/eldev/master/webinstall/github-eldev | sh
```

### Testing
```bash
# Run all tests (standard - minimal output)
eldev test -B

# Run all tests for a specific version of emacs
eldev docker 30 test

# Run all tests with backtraces (debugging)
eldev test

# Run tests with full debugging (CI command)
eldev -p -dtT test

# Run specific test file
eldev test -B --file="test/organizing-test.el"

# Stop on first failure
eldev test -B --stop

# Test specific functions (buttercup selectors)
eldev test -B "clarify"
```

### Build and Compilation
```bash
# Compile all elisp files
eldev compile

# Clean and recompile (recommended for testing)
eldev clean && eldev compile

# Compile with warnings as errors
eldev compile --warnings-as-errors

# Build autoloads (automatic during compile)
eldev compile --set all
```

### Linting and Validation
```bash
# Run all linters (package-lint, checkdoc, etc.)
eldev lint

# Lint specific files
eldev lint --file="org-gtd-core.el"

# List available linters
eldev lint --list

# Check dependencies status
eldev dependencies

# Comprehensive project health check
eldev doctor

# Update copyright notices
eldev update-copyright
```

### Manual Testing Environment
```bash
# Use sandboxed dev environment
HOME="dev/" emacs

# Test autoloads without full package installation
# (Uses dev/init.el configuration)
```

### Container Testing (Multiple Emacs Versions)
```bash
# Build container for specific emacs version
podman build -t stag-28-2 -f dev/Containerfile --build-arg emacs_version=28-2 .

# Run tests in container
eldev -dt docker "localhost/stag-28-2" -C -dtp test
```

## Architecture Overview

org-gtd.el implements David Allen's Getting Things Done (GTD) methodology as an Emacs package using org-mode. The architecture follows GTD's six-step workflow:

### Core Module Structure

**Entry Point**: `org-gtd.el` loads all modules and manages versioning

**Core Infrastructure**:
- `org-gtd-core.el`: Customization variables, macros, context management
- `org-gtd-files.el`: File path management and GTD directory structure
- `org-gtd-mode.el`: Minor mode integration with org-agenda

**GTD Workflow Modules** (organized by GTD steps):
1. **Capture**: `org-gtd-capture.el` - Inbox collection
2. **Process**: `org-gtd-process.el` - Inbox processing loop
3. **Clarify**: `org-gtd-clarify.el` + `org-gtd-wip.el` - Item clarification with WIP buffers
4. **Organize**: `org-gtd-organize.el` + category modules - Transient-based organization dispatcher
5. **Engage**: `org-gtd-agenda.el` - Daily engagement views
6. **Review**: `org-gtd-review.el` + `org-gtd-oops.el` - Review workflows

**Organization Categories** (each has dedicated module):
- Projects: `org-gtd-projects.el` (multi-step)
- Single Actions: `org-gtd-single-action.el`
- Calendar: `org-gtd-calendar.el` (time-specific)
- Delegate: `org-gtd-delegate.el` (waiting for others)
- Incubate: `org-gtd-incubate.el` (someday/maybe)
- Knowledge: `org-gtd-knowledge.el` (reference)
- Habits: `org-gtd-habit.el` (recurring)
- Quick Actions: `org-gtd-quick-action.el` (<2min tasks)

### Key Design Patterns

**Context Management**: `with-org-gtd-context` macro ensures proper org-mode settings across all operations

**Transient UI**: Modern menu interface for organization choices using transient.el

**WIP Buffers**: Dedicated major mode (`org-gtd-wip-mode`) derived from org-mode for clarification workflow

**Modular Organization**: Each GTD category is completely self-contained with its own module

**Version Management**: Sophisticated upgrade system (`org-gtd-upgrades.el`) with user acknowledgment

**Backward Compatibility**: Dedicated module for handling API changes across versions

### File Organization Patterns

GTD files are organized in `org-gtd-directory` (default: `~/gtd/`):
- `inbox.org` - Capture target
- `org-gtd-tasks.org` - Main GTD file
- `org-gtd-incubate.org` - Someday/maybe items
- `org-gtd-calendar.org` - Calendar items

### Testing Architecture

Uses Buttercup framework with comprehensive test coverage:
- **Unit tests**: Per-module testing in `test/*-test.el`
- **Autoload tests**: Separate directory testing lazy loading
- **Integration helpers**: Realistic GTD workflow simulation
- **Fixtures**: Example GTD data for testing

Key testing dependencies: `with-simulated-input`, `dash`

### Key Entry Points

**Interactive Commands** (autoloaded):
- `org-gtd-capture` - Capture to inbox
- `org-gtd-process-inbox` - Process inbox items
- `org-gtd-organize` - Organize item at point
- `org-gtd-engage` - Daily engagement view
- `org-gtd-clarify-item` - Clarify any org heading

**Customization Variables**:
- `org-gtd-directory` - Main GTD directory
- `org-gtd-areas-of-focus` - Life areas (GTD Horizons)
- `org-gtd-organize-hooks` - Functions run during organization

### Dependencies

- **Emacs 27.2+**: Minimum version
- **org 9.6**: Core org-mode functionality
- **org-edna 1.1.2**: Task dependencies
- **org-agenda-property 1.3.1**: Agenda enhancements
- **transient 0.3.7**: Menu interface
- **f 0.20.0**: File utilities

## Documentation

- Info manual: `C-h i m org gtd RET`
- Source: `doc/org-gtd.org` (exports to Texinfo)
- Comprehensive README.org with quick tour

## Memory Annotations

- When using the emacs mcp server, always redefine or eval-buffer, don't use file loads, because of emacs' caching mechanism

## Long-term TODO Items

### Progress Cookie Implementation
- **Issue**: Built-in org-mode progress cookies may not work correctly with flexible project structure
- **Solution**: Implement custom progress tracking that understands task dependencies
- **Priority**: Low - disable built-in cookies first, implement custom solution later
- **Files**: `org-gtd-projects.el` (line 212-217 has current cookie logic)
