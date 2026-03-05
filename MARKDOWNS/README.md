# Markdowns

## Features

Markdowns extend on regular mark down in this and that way... _**FILL THIS OUT**_

### Variables

Store values in variables, e.g., last release year, etc.
- Variables are **always** immutable.
- Variables are scoped - defined in one file does not mean they can be used across files.
- Variables need to be initialized with a value and cannot be null.
- Variables need to be defined before usage.
- Variables are strongly typed.

Something alike `today()` **FILL THIS OUT**

```
Float today = today()
```

<!-- 
Could have: Date parsing to unix timestamp
E.g. `Date release_date = parse_date(12-23-2026)`
(Should it come with a Date type or just use float?)
 -->

#### Usage

To initiate a variable, write the type in title case, then the name of the variable and the value.

The types are:
- Float
- Bool
- String

```mds
Float f = 0.0;

Bool is_release = true;

String author = "SW4";
```

### Glossary

Define abbreviations, people, etc.

Used to ensure no terms, abbreviations or people are mentioned as "should be known" before they are defined clearly.

#### Usage
```mds
Abbr API = "Application Programming Interface";
Abbr CI = "Continuous Integration";
Abbr MDS = "Markdowns Structured Language";
Abbr SW4 = "Software Engineering Group 4";
```
### Conditionals

Conditional rendering. Can use binary comparisons with hardcoded values and variables. Comparisons need to be done between to values of equal type.

#### Usage

Equal operator: `==`

Larger than `>`

Larger than or equal to `>=`

```mds
String releaseDate = "01/04-2026" 
String todaysDate = "02/03-2026"

if(releaseDate <= todaysDate) {
    ## 🚀 Release Notes
    
    Version 2.0.0
    
    New Features:
    - Extended glossary types
    - Added Mathematical Operations
    
    Bug Fixes:
    - The equal operator now evaluates the value, and not type.
} else {
    ## 🚀 Release Notes
    
    Version 1.0.0
    
    New Features:
    - Introduced variables.
    - Introduced glossary.
    - Introduced conditionals
}
```

<!-- 
Could have: 
### Mathematical Operations
### Extended glossary types (abbreviations, actors) 
-->

## Language Guide

To use these features, you need to either do it in a code block at the beginning of your `.mds` file, or do annotations directly in your text.

To initiate a code block, write `§/` and to end it, write `/§`. 

To use annotations, write `§§`.

To write comments inside a code block, you can do single line comments using two slashes `//`. Multi-line comments are not a part of Markdowns.

End each line with a  semicolon `;`.

## Example

This document was compiled from Markdowns. See the `.mds` file here: [README.mds](README.mds)