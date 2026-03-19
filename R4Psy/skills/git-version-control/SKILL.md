---
name: "git-version-control"
description: "Teaches Git/GitHub concepts and executes git commands. Invoke when users ask about version control, need help with git operations, or want to learn R4Psy Chapter 3.2 content."
---

# Git Version Control Skill

## Overview

This skill helps users understand and use Git and GitHub based on the R4Psy course materials (Chapter 3.2). It combines educational content from `slides/chapter_3.Rmd` with practical command execution capabilities.

## When to Invoke

- User asks about version control concepts (版本控制)
- User needs help with Git installation or configuration
- User wants to execute git commands (init, add, commit, push, pull, etc.)
- User needs guidance on GitHub operations (create repo, clone, fork, etc.)
- User encounters git errors or conflicts
- User references R4Psy Chapter 3 or asks about course content

## Capabilities

### 1. Educational Content (Based on chapter_3.Rmd)

#### 1.1 Version Control Concepts (3.2.1)
- **Definition**: 版本控制(Version Control)是一种管理文件修改历史的系统，特别是用于源代码管理。
- **Categories**: 
  - 本地版本控制 (Local): 在用户的个人计算机上运行，管理和跟踪文件或项目的历史版本
  - 远程版本控制 (Remote): 侧重于团队协作和共享资源，依托于服务器或云服务

#### 1.2 Git Fundamentals (3.2.2)
- Git is the most commonly used free and open-source version control tool
- Installation: https://git-scm.com/downloads
- Mac users may need to install homebrew first

**Two Operation Modes**:
- **Terminal**: Full functionality via Git Bash, CMD, R Terminal, etc.
- **GUI Panel**: Visual interface like R Studio Git panel, GitHub Desktop

#### 1.3 GitHub Overview (3.2.3)
- GitHub is the world's largest open-source online software source code hosting platform
- Uses Git for version control
- Acquired by Microsoft in 2018
- Alternatives: Gitee, GitLab, Bitbucket
- Registration: https://github.com

### 2. Git Commands Reference

#### Initialization & Setup
```bash
# Initialize a new repository
git init

# Clone an existing repository
git clone <repository-url>

# Configure user identity
git config --global user.name "Your Name"
git config --global user.email "your.email@example.com"

# Check configuration
git config --list
```

#### Daily Workflow
```bash
# Check repository status
git status

# Stage changes
git add <file>           # Add specific file
git add .                # Add all changes

# Commit changes
git commit -m "Commit message"

# View commit history
git log
git log --oneline        # Compact view
git log --graph          # Visual graph
```

#### Branch Operations
```bash
# List branches
git branch

# Create new branch
git branch <branch-name>

# Switch branch
git checkout <branch-name>
git switch <branch-name>  # Modern syntax

# Create and switch
git checkout -b <branch-name>
git switch -c <branch-name>  # Modern syntax

# Merge branch
git merge <branch-name>

# Delete branch
git branch -d <branch-name>   # Safe delete
git branch -D <branch-name>   # Force delete
```

#### Remote Operations
```bash
# Add remote repository
git remote add origin <repository-url>

# View remotes
git remote -v

# Push to remote
git push origin <branch-name>
git push -u origin <branch-name>  # Set upstream

# Pull from remote
git pull origin <branch-name>

# Fetch updates
git fetch origin

# Push all branches
git push --all origin
```

#### Undo Operations
```bash
# Unstage files
git reset HEAD <file>
git restore --staged <file>  # Modern syntax

# Discard local changes
git checkout -- <file>
git restore <file>           # Modern syntax

# Amend last commit
git commit --amend

# Reset to specific commit
git reset --soft HEAD~1      # Keep changes staged
git reset --mixed HEAD~1     # Keep changes unstaged
git reset --hard HEAD~1      # Discard changes

# Stash changes
git stash
git stash list
git stash pop
git stash drop
```

### 3. GitHub Operations

#### Browser-based Operations
1. **Create Repository**: User profile → Repositories → New
2. **Fork Repository**: Click "Fork" button on any public repo
3. **Pull Requests**: Compare & pull request → Create pull request
4. **Issues**: Create and manage project issues

#### GitHub CLI Integration
```bash
# Install GitHub CLI (if not installed)
# macOS: brew install gh
# Windows: winget install --id GitHub.cli

# Authenticate
gh auth login

# Create repository
gh repo create <repo-name>

# Clone repository
gh repo clone <owner>/<repo>

# Create pull request
gh pr create

# View issues
gh issue list
```

### 4. Authentication Setup

#### SSH Key Setup
```bash
# Generate SSH key
ssh-keygen -t ed25519 -C "your.email@example.com"

# Add to SSH agent
eval "$(ssh-agent -s)"
ssh-add ~/.ssh/id_ed25519

# Copy public key to clipboard
# macOS: pbcopy < ~/.ssh/id_ed25519.pub
# Add to GitHub: Settings → SSH and GPG keys → New SSH key
```

#### Personal Access Token (PAT)
1. GitHub → Settings → Developer settings → Personal access tokens
2. Generate new token (classic) or Fine-grained token
3. Select scopes: repo, workflow, etc.
4. Use token as password when prompted

### 5. Error Handling

#### Common Issues & Solutions

**Merge Conflicts**:
```bash
# Identify conflicted files
git status

# Edit files to resolve conflicts (look for <<<<<<< HEAD)
# Then stage and commit
git add <resolved-files>
git commit -m "Resolve merge conflict"
```

**Permission Denied**:
- Check SSH key configuration
- Verify PAT has correct scopes
- Ensure correct repository access

**Detached HEAD State**:
```bash
# Create new branch from detached state
git checkout -b <new-branch-name>

# Or return to previous branch
git checkout <branch-name>
```

## Execution Guidelines

### Teaching Mode
1. When user asks about concepts, quote directly from chapter_3.Rmd (in Chinese)
2. Provide context and examples to enhance understanding
3. Reference specific sections (3.2.1, 3.2.2, 3.2.3) when applicable

### Command Execution Mode
1. **Safety First**: Always confirm before executing destructive commands (reset --hard, push --force, etc.)
2. **Explain Before Execute**: Describe what the command does before running it
3. **Show Output**: Display command output with explanations
4. **Check Status**: Use `git status` frequently to show current state

### Interactive Guidance
1. **Assess Level**: Ask about user's experience level if unclear
2. **Step-by-Step**: Break complex operations into manageable steps
3. **Verify Understanding**: Confirm user understands each step before proceeding
4. **Provide Alternatives**: Offer both terminal and GUI options when applicable

## Workflow Examples

### First-time Setup
1. Install Git
2. Configure identity
3. Set up SSH key or PAT
4. Verify with `git --version` and test connection

### Starting a New Project
1. Create GitHub repository (browser or gh CLI)
2. Clone locally: `git clone <url>`
3. Add files and make initial commit
4. Push to remote

### Daily Development Loop
1. Pull latest changes: `git pull`
2. Create feature branch: `git checkout -b feature-name`
3. Make changes and commit regularly
4. Push branch: `git push -u origin feature-name`
5. Create pull request on GitHub
6. Merge and clean up branches

## Language Policy

- **SKILL.md**: English (this file)
- **User Interaction**: Chinese (consistent with chapter_3.Rmd source)
- **Command Output**: Preserve original language (English for git commands)
- **Explanations**: Chinese with English terms in parentheses when helpful

## Safety Warnings

⚠️ **Never**:
- Execute `git push --force` without explicit user confirmation
- Run `git reset --hard` without checking for uncommitted changes
- Share or log authentication credentials
- Assume repository state without checking first

✅ **Always**:
- Check `git status` before and after operations
- Confirm destructive actions
- Explain potential side effects
- Offer to create backups when appropriate
