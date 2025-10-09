# Contributing to the Potomac River Kayaking Safety Analysis

Thank you for considering contributing to the Potomac River Kayaking Safety Analysis project! This document provides guidelines and instructions for contributing.

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.

## Development Workflow

### Setting Up the Development Environment

1. Fork the repository on GitHub
2. Clone your fork to your local machine
   ```
   git clone https://github.com/your-username/potomac.git
   cd potomac
   ```
3. Set up the upstream remote
   ```
   git remote add upstream https://github.com/original-owner/potomac.git
   ```
4. Install development dependencies
   ```r
   # In R
   install.packages("renv")
   renv::restore()
   ```

### Making Changes

1. Create a new branch for your feature or bugfix
   ```
   git checkout -b feature/your-feature-name
   ```
   or
   ```
   git checkout -b bugfix/issue-description
   ```

2. Make your changes following the coding style guidelines

3. Add tests for your changes

4. Run tests and checks
   ```r
   # In R
   devtools::test()
   devtools::check()
   ```

5. Commit your changes with a descriptive commit message
   ```
   git commit -m "feat(component): add your feature description"
   ```

6. Push your branch to your fork
   ```
   git push origin feature/your-feature-name
   ```

7. Create a pull request from your fork to the main repository

## Commit Message Guidelines

Follow the structured commit message format:

```
<type>(<scope>): <subject>

<body>

<footer>
```

Where:
- **type**: feat, fix, docs, style, refactor, test, chore
- **scope**: predictor, dashboard, analysis, etc.
- **subject**: Short description of the change
- **body**: Detailed description
- **footer**: Breaking changes, references to issues

Examples:
- `feat(predictor): add water temperature to safety calculation`
- `fix(dashboard): correct error in trend calculation`
- `docs(readme): update installation instructions`

## Pull Request Process

1. Update the README.md or documentation with details of changes if appropriate
2. Update the version number in DESCRIPTION following semantic versioning
3. The PR will be merged once it passes all checks and receives approval

## Coding Style Guidelines

This project follows the [tidyverse style guide](https://style.tidyverse.org/) for R code:

- Use snake_case for variable and function names
- Use `<-` for assignment
- Maximum 80 characters per line
- Use 2 spaces for indentation (no tabs)
- Include spaces around operators

## Documentation

- Use roxygen2 for function documentation
- Include examples in function documentation
- Keep documentation up to date with code changes

## Testing

- Write tests for all new features and bug fixes
- Ensure all tests pass before submitting a pull request
- Aim for high code coverage

## Questions?

If you have any questions or need help, please open an issue or contact the maintainers.