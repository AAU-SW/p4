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

Something alike `Today()` _**FILL THIS OUT**_

```
$/
Int today = Today()
/$
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
- Int
- Bool
- String

Important: Date type is not supported, but can be represented as a unix timestamp using the `Int` type.

```mds
$/
Float f = 0.0;

Int counter = 3;

Bool is_release = true;

String author = "SW4";
/$
```

### Glossary

Define abbreviations, people, etc.

Used to ensure no terms, abbreviations or people are mentioned as "should be known" before they are defined clearly.

#### Usage

##### Definitions

```mds
$/
Abbr API = "Application Programming Interface";
Abbr CI = "Continuous Integration";
Abbr MDS = "Markdowns Structured Language";
Abbr SW4 = "Software Engineering Group 4";
/$
```

##### Usage

```mds
$$ Req(API)
We built this using the weather data API.
```

### Conditionals

Conditional rendering. Can use binary comparisons with hardcoded values and variables. Comparisons need to be done between to values of equal type.

#### Usage

Equal operator: `==`

Larger than `>`

Larger than or equal to `>=`

```mds
$/
Int releaseDate = 1774994400;
Int todaysDate = 1772406000;
/$

$/ if (releaseDate <= todaysDate) { /$
    ## 🚀 Release Notes
    
    Version 2.0.0
    
    New Features:
    - Extended glossary types
    - Added Mathematical Operations
    
    Bug Fixes:
    - The equal operator now evaluates the value, and not type.
$/ } else { /$
    ## 🚀 Release Notes
    
    Version 1.0.0
    
    New Features:
    - Introduced variables.
    - Introduced glossary.
    - Introduced conditionals
$/ } /$

```

<!-- 
Could have: 
### Extended glossary types (abbreviations, actors) 
-->

### Built-in Functions

- `Today()`: returns the current date as a unix timestamp
- `Req()`: used to reference glossary definitions. Takes the name of the definition as an argument. E.g. `Req(API)` and checks if said glossary definition has been defined. If it has not, an error will be thrown, otherwise the Markdown will render as normal. The content of the definition is not rendered, but it is used to check if the definition exists. This is to ensure that all terms are defined before they are used.

## Language Guide

To use these features, you need to either do it in a code block or do annotations directly in your text.

To initiate a code block, write `$/` and to end it, write `/$`. 

To use annotations, write `$$`. Annotations are always ended at the end of the line, so there is no need to write an end annotation.

To write comments inside a code block, you can do single line comments using two slashes `//`. Multi-line comments are not a part of Markdowns.

End each line with a  semicolon `;`.

The expressions between curly brackets in if-statements are treated as regular markdown, but the content is only rendered if the condition is satisfied.

Templating: If you want to use a variable in your markdown, you can do so by writing the variable name between curly brackets with a $ in front. E.g. `${variable_name}`. This will render the value of the variable in the markdown. This can also be used in conjunction with conditionals. E.g. 

**Everything not in a code block, annotation or template is just regular markdown.**

```mds
$/
String version = "2.0.0";
Int releaseDate = 1774994400;
Int todaysDate = Today();
/$

$/ if(releaseDate <= todaysDate) { /$
  ## 🚀 Release Notes - Version ${version} 
$/ } /$
```


## Example

This document was compiled from Markdowns. See the `.mds` file here: [README.mds](README.mds)
