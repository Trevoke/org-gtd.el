# Org-Mode Properties Documentation

## Overview
Properties are key-value pairs associated with entries in an Org document. They have two primary use cases:

1. **Descriptive Categorization**: Instead of using tags like 'release_1', you can use a property 'Release' with values like '1.0' or '2.0'.

2. **Basic Database Tracking**: Properties can track detailed information about items, such as tracking music CD details (album, artist, release date, track count).

Properties can be:
- Associated with a single entry
- Applied to every entry in a tree  
- Set for an entire buffer

## Property API Functions

### Property Retrieval and Setting
- `org-entry-get(pom, property, inherit)`: Retrieve a property value
- `org-entry-put(pom, property, value)`: Set a property value
- `org-entry-delete(pom, property)`: Delete a specific property

### Multi-Value Property Functions
- `org-entry-get-multivalued-property(pom, property)`: Get values as a list
- `org-entry-put-multivalued-property(pom, property, values)`: Set multiple values
- `org-entry-add-to-multivalued-property(pom, property, value)`: Add a value
- `org-entry-remove-from-multivalued-property(pom, property, value)`: Remove a value
- `org-entry-member-in-multivalued-property(pom, property, value)`: Check if value exists

### Additional Utility Functions
- `org-entry-properties()`: Get all properties for an entry
- `org-buffer-property-keys()`: List all property keys in current buffer
- `org-insert-property-drawer()`: Insert a property drawer

The `pom` parameter typically represents the point-or-marker for the Org mode entry being modified.

## Key Insights for Multi-Value Properties
- Org-mode has built-in support for multi-value properties
- Space-separated values in properties are handled automatically by the API
- The multi-value functions provide clean programmatic access to list-style properties
- This makes implementing BLOCKED_BY and BLOCKS properties with multiple IDs straightforward