# PROJECT KNOWLEDGE BASE

**Generated:** 2026-02-24 20:17:43 CST
**Commit:** 550749c
**Branch:** main

## OVERVIEW
Graduate-level R course materials (R for Psychology) with chapter slides in R Markdown, a single Quarto revealjs lecture, and pre-rendered HTML/PDF outputs.

## STRUCTURE
```
R4Psy/
├── chapter_*.Rmd            # Primary slide sources (xaringan)
├── chapter_*.html           # Rendered slide outputs
├── chapter_*.pptx           # Slide decks for early chapters
├── chapter_14.qmd           # Quarto revealjs lecture
├── Demo.Rmd                 # papaja demo manuscript
├── css/                     # Xaringan CSS themes
├── data/                    # Datasets + research folder templates
├── env/                     # Environment setup guide + screenshots
├── homeworks/               # Yearly homework folders
├── libs/                    # Xaringan/HTML dependencies (vendored)
├── output/                  # Generated outputs by chapter
├── picture/                 # Chapter images
├── chapter_13_guest_Du_XK/  # Guest lecture: network analysis
└── chapter_14_guest_Liu_Z/  # Guest lecture: meta-analysis
```

## WHERE TO LOOK
| Task | Location | Notes |
|------|----------|-------|
| Edit chapter slides | chapter_*.Rmd | Xaringan slides via `xaringan::moon_reader` |
| Render chapter slides | chapter_*.Rmd | Produces chapter_*.html in root |
| Quarto lecture | chapter_14.qmd | Revealjs, uses `theme.scss` |
| papaja manuscript | Demo.Rmd, chapter_13_papaja.Rmd | Renders to PDF/TeX |
| Data used in examples | data/match, data/penguin | Loaded via `here::here()` |
| Xaringan styling | css/Font_Style.css, css/zh-CN.css, css/Custumed_Style.css | Applied via YAML `css:` |
| Revealjs styling | theme.scss | Applied in `chapter_14.qmd` |
| Env setup guide | env/env_init.md | Screenshot-heavy tutorial |
| Guest network analysis | chapter_13_guest_Du_XK | `dag/`, `ggm/` scripts |
| Guest meta-analysis | chapter_14_guest_Liu_Z | `code/`, `datasheets/` |

## CONVENTIONS
- Slides use `output: xaringan::moon_reader` with `css: [default, css/... ]` and `lib_dir: libs`.
- Chapter code uses `here::here()` for data paths; avoid absolute paths.
- Chunk labels often include section numbers (e.g., `5.1`, `xaringan-panelset`).
- Packages loaded with `pacman::p_load(...)` or `library(...)`.

## ANTI-PATTERNS (THIS PROJECT)
- Do not hand-edit rendered artifacts (`chapter_*.html`, `*_files/`, `output/`); re-render from source instead.
- Do not edit `libs/` (vendored dependencies) unless intentionally updating generated outputs.
- Avoid absolute file paths in R code; use `here::here()` consistently.

## UNIQUE STYLES
- Xaringan CSS uses custom Chinese/English typography in `css/`.
- Quarto revealjs theme in `theme.scss` sets fonts/colors for Chapter 14.

## COMMANDS
```bash
Rscript -e "rmarkdown::render('chapter_3.Rmd')"
Rscript -e "rmarkdown::render('Demo.Rmd')"
quarto render chapter_14.qmd
```

## NOTES
- No CI/build automation; rendering is manual via RStudio or CLI.
- Repo includes many generated assets and large binaries.
- Shared helper functions are defined inline in chapters (e.g., `chapter_7.Rmd` for `convert_data_types`, `chapter_9_supplementary.Rmd` for datawizard-style helpers); there is no central utils file.

---

# AI INTERACTION GUIDE FOR R4PSY COURSE

## PURPOSE
This section provides guidelines for students and AI assistants to interact effectively when working through the R4Psy course materials, particularly for Chapter 5 and beyond.

## GENERAL PRINCIPLES

### 1. Understanding Over Memorization
- Focus on understanding **concepts** rather than memorizing syntax
- AI can help generate code, but you need to understand what the code does
- Always ask "why" when AI provides a solution

### 2. Iterative Problem Solving
- Start with simple questions, then build complexity
- Don't try to solve everything in one prompt
- Use AI's responses to refine your understanding

### 3. Verification and Testing
- Always test AI-generated code with your own data
- Check results against expected outcomes
- Ask AI to explain code logic when unclear

---

## PROMPT TEMPLATE STRUCTURE

When asking AI for help with R programming, use this structure:

### Template Format
```
**Background:**
[Describe your current task or problem context]

**Data:**
[Describe your data structure: variables, types, size]

**Requirements:**
[Specific operations or analyses you need]

**Constraints:**
[Any limitations or special requirements]
```

### Example Prompt
```
**Background:**
I'm working on Chapter 5 exercises about data types and structures.

**Data:**
I have a data frame loaded from:
`here::here("slides", "data", "penguin", "penguin_rawdata.csv")`
It contains columns: age (numeric), Site (character), weight_kg (numeric), height_cm (numeric)

**Requirements:**
1. Filter data where Site is "Tsinghua" and age > 25
2. Calculate BMI for these participants
3. Create a summary table by sex

**Constraints:**
- Use data.table for efficiency
- Handle missing values appropriately
- Round numeric results to 2 decimal places
```

---

## CHAPTER-SPECIFIC PROMPTS

### Chapter 5: Data Types & Data Structures

#### For Data Type Questions
```
**Background:**
I'm learning about R data types in Chapter 5.

**Question:**
[Your specific question about numeric, character, logical, or factor types]

**Example:**
"How do I check if a column in my data frame is numeric or character? 
What function should I use to convert between types?"
```

#### For Data Structure Questions
```
**Background:**
I'm working with different data structures (vectors, matrices, data frames, data.table).

**Current Code:**
[Paste your current code if any]

**Problem:**
[Describe what's not working or what you're trying to achieve]

**Expected Output:**
[Describe what you want the result to look like]
```

#### For Function Questions
```
**Background:**
I need to create/modify a function for [specific task].

**Function Requirements:**
- Input: [describe parameters]
- Output: [describe return value]
- Special handling: [missing values, error checking, etc.]

**Example Usage:**
[Show how you want to call the function]
```

---

## ITERATIVE OPTIMIZATION STRATEGIES

### Strategy 1: Incremental Complexity
**Round 1 - Basic Question:**
```
"How do I create a data frame with 3 columns: id, name, and score?"
```

**Round 2 - Add Complexity:**
```
"Good! Now how do I add a new column 'grade' based on the score values?"
```

**Round 3 - Refine:**
```
"Can you show me how to do this more efficiently using data.table?"
```

### Strategy 2: Error Resolution
**Initial Prompt:**
```
"I'm getting an error when running this code:
[Paste code and error message]

What's wrong and how do I fix it?"
```

**Follow-up:**
```
"The fix worked, but can you explain why the error occurred 
and how to prevent similar errors in the future?"
```

### Strategy 3: Alternative Approaches
**Initial Request:**
```
"How do I calculate the mean of a column with missing values?"
```

**Follow-up:**
```
"What are the different ways to handle missing values in R? 
Show me 2-3 approaches with pros and cons."
```

---

## BEST PRACTICES FOR AI INTERACTION

### DO:
✅ **Be specific about data paths**
- Use: `here::here("slides", "data", "penguin", "penguin_rawdata.csv")`
- Don't use: `"data/penguin_rawdata.csv"` (relative paths may fail)

✅ **Provide context**
- Mention which chapter/exercise you're working on
- Describe what you've already tried

✅ **Ask for explanations**
- "Can you explain what this line does?"
- "Why do we need to use na.rm = TRUE?"

✅ **Request code comments**
- "Please add comments explaining each step"
- "Can you show me the code with detailed annotations?"

✅ **Verify understanding**
- "Is my understanding correct: [your interpretation]?"
- "Can you confirm if this approach is appropriate for my use case?"

### DON'T:
❌ **Don't ask vague questions**
- Bad: "My code doesn't work"
- Good: "I'm getting error 'object not found' when running [code snippet]"

❌ **Don't skip prerequisites**
- Make sure you've loaded required packages
- Ensure data is properly loaded before asking for operations

❌ **Don't ignore warnings**
- Ask AI to explain warning messages
- Understand potential issues before proceeding

❌ **Don't copy-paste without understanding**
- Always read and understand AI-generated code
- Test with small examples first

---

## COMMON PATTERNS FOR CHAPTER 5

### Pattern 1: Data Import and Inspection
```r
# Standard import pattern
df <- bruceR::import(here::here("slides", "data", "penguin", "penguin_rawdata.csv"))

# Inspection commands to request
str(df)           # Structure
head(df)          # First few rows
summary(df)       # Summary statistics
names(df)         # Column names
```

### Pattern 2: Data Type Conversion
```r
# Ask AI: "How do I convert between these types?"
as.numeric()      # To numeric
as.character()    # To character
as.logical()      # To logical
as.factor()       # To factor
```

### Pattern 3: Data Structure Operations
```r
# Vector operations
v <- c(1, 2, 3, 4, 5)
v[v > 3]          # Filtering

# Data frame operations
df$new_col <- df$col1 + df$col2    # Add column
subset(df, condition)              # Filter rows

# data.table operations
dt[condition, .(columns), by = group]
```

### Pattern 4: Function Creation
```r
# Template for function questions
my_function <- function(x, param = default) {
  # Input validation
  if (!is.numeric(x)) stop("x must be numeric")
  
  # Core logic
  result <- [operations]
  
  # Return
  return(result)
}
```

---

## TROUBLESHOOTING GUIDE

### When AI Code Doesn't Work

**Step 1: Check Prerequisites**
```
Ask AI: "What packages do I need to run this code? 
How do I check if they're installed?"
```

**Step 2: Verify Data**
```
Ask AI: "How do I check if my data frame has the right structure 
for this operation?"
```

**Step 3: Debug Step-by-Step**
```
Ask AI: "Can you break down this code into smaller steps 
so I can see where the problem occurs?"
```

### When Results Look Wrong

**Step 1: Validate Logic**
```
Ask AI: "Can you explain the logic behind this calculation? 
I expected [X] but got [Y]."
```

**Step 2: Check Data Issues**
```
Ask AI: "Could missing values or data types be affecting my results? 
How do I check?"
```

**Step 3: Request Alternatives**
```
Ask AI: "Is there another way to achieve the same result? 
I want to verify if this approach is correct."
```

---

## EXAMPLE CONVERSATIONS

### Example 1: Creating a Function
**Student:** "I need to create a function that calculates BMI from weight and height."

**Optimal Prompt:**
```
I need to create a BMI calculation function with these requirements:
- Function name: calc_bmi
- Parameters: weight (in kg), height (in cm)
- Output: BMI value (weight / height_m^2)
- Should handle:
  * Input validation (numeric, positive values)
  * Missing values
  * Rounding to 2 decimal places

Please show me:
1. The function code with comments
2. Example usage with test data
3. How to handle error cases
```

### Example 2: Data Manipulation
**Student:** "How do I filter and summarize data?"

**Optimal Prompt:**
```
I'm working with the penguin dataset from:
`here::here("slides", "data", "penguin", "penguin_rawdata.csv")`

I need to:
1. Filter participants from "Tsinghua" site
2. Group by sex
3. Calculate mean and SD of age for each group
4. Create a clean summary table

Please provide:
- Code using base R
- Code using data.table
- Explanation of differences
```

---

## CHAPTER 5 EXERCISE PROMPTS

### Exercise 1: Data Structure Creation
```
**Task:** Create different data structures
**Prompt Template:**
"I need to create a [vector/matrix/data frame] with [specific characteristics].
Please show me:
1. How to create it
2. How to verify its structure
3. How to access/modify elements"
```

### Exercise 2: Data Frame Operations
```
**Task:** Manipulate data frames
**Prompt Template:**
"I have a data frame loaded from [path].
I need to [filter/add columns/transform] based on [conditions].
Please provide code that:
1. Shows each step clearly
2. Handles potential errors
3. Verifies the results"
```

### Exercise 3: Using Functions
```
**Task:** Apply functions to data
**Prompt Template:**
"I need to apply [function name] to my data.
The function should [specific behavior].
Please show me:
1. How to use existing functions
2. How to create a custom version
3. How to compare results"
```

### Exercise 4: Custom Functions
```
**Task:** Create custom functions
**Prompt Template:**
"I need to create a function that [specific purpose].
Requirements:
- Input: [parameters]
- Output: [return type]
- Special handling: [edge cases]

Please provide:
1. Function definition with comments
2. Example usage
3. Test cases"
```

### Exercise 5: Mixed Type Data
```
**Task:** Handle mixed data types
**Prompt Template:**
"I have a data frame with multiple data types:
- Numeric columns: [list]
- Character columns: [list]
- Need to create: [new columns]

Please show me how to:
1. Identify column types programmatically
2. Apply type-specific operations
3. Create derived columns based on conditions"
```

---

## EVALUATION CRITERIA

When reviewing AI-generated code, check:

### Functionality
- [ ] Does the code run without errors?
- [ ] Does it produce expected results?
- [ ] Does it handle edge cases?

### Readability
- [ ] Are variable names clear?
- [ ] Is the logic easy to follow?
- [ ] Are there helpful comments?

### Efficiency
- [ ] Is the approach appropriate for data size?
- [ ] Are there unnecessary steps?
- [ ] Could it be simplified?

### Best Practices
- [ ] Uses `here::here()` for paths?
- [ ] Handles missing values?
- [ ] Includes input validation?
- [ ] Follows R conventions?

---

## GETTING HELP

### When You're Stuck
1. **Describe the problem clearly**
   - What you're trying to do
   - What you've already tried
   - What error or unexpected result you're getting

2. **Provide minimal reproducible example**
   - Small dataset or use built-in data
   - Minimal code that reproduces the issue

3. **Ask specific questions**
   - "Why does this error occur?"
   - "How do I fix this specific issue?"
   - "What's the best approach for this scenario?"

### Resources to Mention
- Chapter number and section
- Exercise number if applicable
- Error messages (copy-paste exactly)
- Your R version and package versions if relevant

---

## UPDATING THIS GUIDE

This guide should be updated when:
- New chapters are added with AI interaction components
- Common student questions reveal gaps in the guide
- New best practices emerge for AI-assisted coding
- Course materials are significantly revised

To suggest updates, note:
- Which section needs updating
- What new content should be added
- Why it would be helpful for students
