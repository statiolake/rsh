# RSH Development Rules

## Code Style

### Comments
- Avoid unnecessary comments that explain what the code is doing
- Write clean, self-explanatory code that doesn't require explanatory comments
- Only include "why" comments when the reasoning cannot be expressed in code
- All comments must be written in English
- Focus on code clarity over comment quantity

### Function Design
- Avoid duplicate logic and repeated conditional statements
- Use descriptive function names that clearly indicate their purpose
- Create specialized functions for specific use cases rather than generic ones

## Commit Guidelines

### Conventional Commits
- Use conventional commit format in English
- Format: `type: description`
- Include detailed bullet points for complex changes
- Examples:
  - `fix: allow underscores in environment variable names`
  - `feat: add new command parser`
  - `refactor: simplify lexer logic`

### Commit Types
- `fix`: Bug fixes
- `feat`: New features
- `refactor`: Code refactoring
- `docs`: Documentation changes
- `test`: Test additions or modifications

### Commit Granularity
- Make atomic commits that represent a single logical change
- Each commit should build and pass tests
- Separate different types of changes into different commits
- Avoid mixing feature implementation with refactoring in the same commit
- Commit frequently to maintain clear development history

### Pre-Commit Checks
- Always run `git status` to review staged files before committing
- **MANDATORY**: Use `git diff --cached` to review changes that will be committed
- Verify that only intended changes are included in the commit
- Never commit without first reviewing the diff output
- This step is critical for catching unintended changes and ensuring commit quality

## Parser/Lexer Guidelines

### Error Handling
- Provide meaningful error messages
- Support error recovery mode when appropriate
- Include span information for error reporting

## Testing
- Test all changes before committing
- Ensure builds pass without errors
- Verify functionality with realistic use cases

## File Organization
- Keep related functionality in appropriate modules
- Use clear, descriptive file and function names
- Maintain consistent code structure across the project

## Task Completion Workflow

### Commit Before Task Completion
- Always commit changes before reporting task completion
- Complete all necessary commits as part of the task itself
- Never report task completion without first committing changes
- Follow the complete commit workflow:
  1. Run `git status` to review staged files
  2. Run `git diff --cached` to review changes (MANDATORY)
  3. Commit with proper conventional commit message
  4. Only then report task completion

## Rule Maintenance
- After each coding task with Roo Code, update these rules under .roo/rules based on new guidelines and feedback
- Incorporate task-specific lessons learned into general development principles
- Focus on guidelines that improve code quality and development workflow
- Avoid including task-specific implementation details in the general rules
