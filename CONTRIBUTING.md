# Contributing to Xolmis

Thank you for your interest in contributing to **Xolmis**!  
We welcome contributions from the community to help improve ornithological data management tools.

## How to Contribute

### Reporting Issues

- Use the [GitHub Issues](https://github.com/cbeier-studio/Xolmis/issues) page to report bugs, request features, or suggest improvements.
- Provide clear steps to reproduce bugs and include screenshots or logs when possible.
- For security-related issues, please follow our [Security Policy](SECURITY.md) and report privately.

### Suggesting Features

- Open a feature request issue describing the problem and the proposed solution.
- Explain how the feature would benefit users and provide examples if possible.

### Submitting Code

1. Fork the repository and create your branch from `main`.
2. Follow the coding style guidelines:
   - Pascal code should use **Lazarus/Free Pascal conventions**.
   - Keep functions small and focused.
   - Document public methods and types using **FPDoc**.
3. Add or update unit tests when applicable.
4. Ensure all tests pass before submitting.
5. Commit messages should be clear and descriptive:
   - Use the format: `feat:`, `fix:`, `docs:`, `test:`, `refactor:`.
   - Example: `fix: handle missing EXIF metadata in AddImage`.

### Pull Requests

- Ensure your PR is linked to an issue (if applicable).
- Provide a clear description of the changes and their purpose.
- Keep PRs focused: avoid mixing unrelated changes.
- PRs will be reviewed by maintainers before merging.

## Development Setup

Please refer to the official Wiki for instructions on setting up your development environment:  

- [Setting Up Your Development Environment](https://github.com/cbeier-studio/Xolmis/wiki/Setting-Up-Your-Development-Environment)
- [Style Guide and Code Review](https://github.com/cbeier-studio/Xolmis/wiki/Style-Guide-and-Code-Review)
- [Building Xolmis](https://github.com/cbeier-studio/Xolmis/wiki/Building-Xolmis)

## Documentation

- All new code must include FPDoc comments for functions, classes, and types.
- Update `CHANGELOG.md` when adding new features or fixing bugs.
- If you add new modules, update the documentation in `docs/`.

## Community Guidelines

- Be respectful and constructive in discussions.
- Follow the [Code of Conduct](https://github.com/cbeier-studio/Xolmis/blob/main/CODE_OF_CONDUCT.md).
- Contributions are licensed under the same license as the project (GPL-3.0).

## Getting Started

If you’re unsure where to begin, check the open issues labeled `good first issue`.
We encourage new contributors to start with documentation, tests, or small bug fixes.

Thank you for helping make **Xolmis** better!
