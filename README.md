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

Fill out

### Conditionals

Conditional rendering. Can use binary comparisons with hardcoded values and variables. Comparisons need to be done between to values of equal type.

#### Usage

Equal operator: `==`

Larger than `>`

Larger than or equal to `>=`

to be continued.

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

