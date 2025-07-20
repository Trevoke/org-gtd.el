# 📋 Specification: GTD-DAG-001

## 📋 Task Overview

Implement a directed acyclic graph (DAG) dependency system for org headings in org-gtd.el that enables hierarchical task blocking relationships. Every org heading will receive unique identification and dependency tracking capabilities through "blocks" and "blocked by" properties.

## 🎯 Objectives

- **🆔 Universal ID Assignment**: Every org heading receives a unique identifier combining date-based root and human-friendly slug
- **🔗 Dependency Tracking**: Enable parent-child blocking relationships between tasks through property lists
- **⚡ Automated State Management**: Automatically transition blocked tasks to NEXT status when dependencies are resolved
- **👤 User Experience**: Provide intuitive interface for users to define and manage task dependencies

## ✅ Requirements

### ID Generation Requirements
- **GIVEN** an org heading exists **WHEN** the system processes it **THEN** it receives a unique ID property
- **GIVEN** ID generation **WHEN** creating identifier **THEN** use date-based root combined with human-friendly slug
- **GIVEN** multiple headings **WHEN** generating IDs **THEN** ensure uniqueness across the entire org system

### Dependency Property Requirements
- **GIVEN** an org heading **WHEN** adding dependency properties **THEN** include "blocks" property for child task IDs
- **GIVEN** an org heading **WHEN** adding dependency properties **THEN** include "blocked by" property for parent task IDs
- **GIVEN** dependency properties **WHEN** storing values **THEN** maintain as lists of task IDs

### Org-Edna Integration Requirements
- **GIVEN** a task completion **WHEN** org-edna trigger fires **THEN** locate all IDs in the completed task's "blocks" list
- **GIVEN** blocked tasks identified **WHEN** checking dependencies **THEN** verify if tasks have remaining blocking parents
- **GIVEN** unblocked tasks **WHEN** no blocking parents remain **THEN** automatically change status to NEXT
- **GIVEN** current org-edna behavior **WHEN** extending functionality **THEN** preserve existing next-sibling-to-next behavior

### User Experience Requirements
- **GIVEN** users need dependency entry **WHEN** defining relationships **THEN** provide intuitive interface for specifying blocks/blocked-by relationships
- **GIVEN** dependency visualization **WHEN** users review tasks **THEN** clearly display blocking and blocked relationships
- **GIVEN** dependency changes **WHEN** users modify relationships **THEN** maintain data consistency across affected tasks

## 🛠️ Implementation Details

### ID System Architecture
- **Date-Based Root**: Timestamp or date component ensuring temporal uniqueness
- **Human-Friendly Slug**: Readable component derived from heading text or user input
- **Uniqueness Guarantee**: System-wide collision prevention mechanism

### Property Structure
```org
* Task Heading
:PROPERTIES:
:ID: 2024-001-project-setup
:BLOCKS: 2024-002-feature-dev 2024-003-testing
:BLOCKED_BY: 2024-000-requirements
:END:
```

### Org-Edna Trigger Enhancement
- **Current Behavior**: Changes next sibling to NEXT status
- **Enhanced Behavior**: 
  - Identify all task IDs in completed task's BLOCKS list
  - For each blocked task, check BLOCKED_BY list
  - Remove completed task ID from BLOCKED_BY lists
  - If BLOCKED_BY becomes empty, transition to NEXT status

### Dependency Management
- **Bidirectional Consistency**: When A blocks B, ensure B lists A in blocked-by
- **Cycle Prevention**: Validate that dependency additions don't create circular references
- **Orphan Handling**: Manage cases where referenced task IDs no longer exist

## 📊 Success Criteria

### Core Functionality
- ✅ Every org heading automatically receives unique ID property
- ✅ BLOCKS and BLOCKED_BY properties correctly store task ID lists
- ✅ Org-edna trigger successfully processes dependency lists
- ✅ Blocked tasks automatically transition to NEXT when unblocked
- ✅ Dependency relationships maintain bidirectional consistency

### User Experience
- ✅ Users can easily define blocking relationships between tasks
- ✅ Visual indication of task dependency status
- ✅ Intuitive interface for dependency modification
- ✅ Clear feedback when dependency changes affect task states

### Data Integrity
- ✅ No circular dependencies can be created
- ✅ Orphaned task references are handled gracefully
- ✅ Property consistency maintained across org file operations
- ✅ ID uniqueness preserved during file operations

## ⚠️ Constraints & Risks

### Technical Constraints
- **Org File Compatibility**: Must maintain compatibility with standard org-mode functionality
- **Performance Impact**: ID generation and dependency checking must not significantly slow org operations
- **Memory Usage**: Dependency tracking structures must be memory efficient for large org files

### Data Integrity Risks
- **Circular Dependencies**: Users might inadvertently create dependency cycles
- **Orphaned References**: Task deletion might leave dangling ID references
- **Concurrent Modifications**: Multiple users editing shared org files could create inconsistencies

### User Experience Risks
- **Complexity Overhead**: Dependency management might overwhelm users with simple workflows
- **Learning Curve**: New concepts require user education and documentation
- **Migration Challenges**: Existing org-gtd workflows need smooth transition path

## 🔧 Technical Context

### Existing Org-GTD Integration
- **Current Workflow**: Tasks progress through NEXT → ACTIVE → DONE states
- **Org-Edna Integration**: Trigger system handles state transitions on completion
- **File Structure**: Maintains org-mode property drawer compatibility

### Architecture Alignment
- **Property System**: Leverage org-mode's native property infrastructure
- **State Management**: Integrate with existing org-gtd state machine
- **Trigger System**: Extend current org-edna trigger patterns

### Dependency Considerations
- **Org-Mode**: Core org-mode functionality and property handling
- **Org-GTD**: Existing workflow and state management system
- **Org-Edna**: Current trigger and automation capabilities

## 📈 Validation Strategy

### Functional Testing
- **ID Generation**: Verify unique ID assignment across all scenarios
- **Dependency Logic**: Test blocking/unblocking behavior with various dependency graphs
- **State Transitions**: Validate automatic NEXT transitions when dependencies resolve
- **Data Consistency**: Ensure bidirectional relationship integrity

### Integration Testing
- **Org-Edna Compatibility**: Verify enhanced triggers work with existing automation
- **Org-GTD Workflow**: Confirm dependency system integrates with current task management
- **File Operations**: Test behavior during org file save/load/export operations

### User Experience Testing
- **Dependency Entry**: Validate ease of defining and modifying relationships
- **Visual Feedback**: Confirm clear indication of dependency status
- **Error Handling**: Test user experience when dependency conflicts arise
- **Performance**: Measure impact on org file operations and responsiveness

### Edge Case Validation
- **Large Dependency Networks**: Test performance with complex dependency graphs
- **File Corruption Recovery**: Validate graceful handling of malformed dependency data
- **Migration Scenarios**: Test upgrade path from non-dependency org-gtd setups
- **Concurrent Access**: Validate behavior with simultaneous file modifications