# Contributing Guidelines

This repository supports collaboration between a **supervisor** and **students** developing R analysis code.  
To avoid conflicts and ensure a clear development process, we follow a structured **Git branching workflow**.

Please read these instructions before contributing.

---

# 📚 Overview of the Workflow

We maintain the following Git branches:

- **`main`** → stable, production-ready code  
- **`develop`** → integration branch for all ongoing work  
- **`develop-studentGITHUBNAME`** → *one branch per student*, created by the supervisor  

Workflow summary:

1. Repository contains `main` and `develop`.
2. Supervisor creates `develop-student1githubname` rooted in `develop`.
3. Student clones repository, works on their branch, and pushes updates.
4. Supervisor creates a pull request (PR) from the student branch into `develop`.
5. Student continues committing and pushing updates.
6. The cycle repeats until the work is complete.

Additional help:  
- Check [the DCC Guides](https://tu-delft-dcc.github.io/) pages on [Collaboration](https://tu-delft-dcc.github.io/docs/software/development_workflow/collaboration.html) and [Branch Management](https://tu-delft-dcc.github.io/docs/software/development_workflow/branch_management.html)
---

# 🔧 Tool Setup Instructions

You may work with **GitHub Desktop** (recommended for beginners) or via **terminal** (Mac Terminal, Windows WSL, or Git Bash).

Below are step‑by‑step installation instructions plus links for extra guidance.

---

## 🖥️ Option A: GitHub Desktop (Windows or macOS)

**Recommended for new Git users.**

### Installation
1. Download: https://desktop.github.com  
2. Install and log in with your GitHub account.

Additional help:  
- **Getting started with GitHub Desktop**: https://docs.github.com/en/desktop/overview/getting-started-with-github-desktop
---

## 🪟 Option B: Windows Subsystem for Linux (WSL)

WSL provides a full Linux environment. For installation check [the official Microsoft guidelines](https://learn.microsoft.com/en-us/windows/wsl/install) or [the Carpentries course setup instructions](https://carpentries.github.io/workshop-template/#shell-install-wsl).

## 🐧 Option C: Git Bash (Windows)
Simpler alternative to WSL for users comfortable with Bash. For installation check [the Carpentries course setup instructions](https://carpentries.github.io/workshop-template/#shell-install-gitbash)

## 🍏 Option D: macOS Terminal

macOS typically includes Git by default.

### Verify Git:

``` bash
git --version
```

If prompted to install developer tools, accept.

Additional help:  
- If you need or prefer to communicate with the remote Github repo using **SSH connection**: https://coderefinery.github.io/installation/ssh/

---

# 🧑‍🏫 Supervisor Workflow

## Step 1 — Create Student Branch

1. Go to the GitHub repository page.
2. Open the branch dropdown.
3. Create a new branch named:

```
develop-studentGITHUBNAME
```

4. Ensure the base branch is:

```
develop
```

5. Inform the student that the branch is ready for use.

Branching help: https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/managing-branches-in-your-repository
---

## Step 2 — Create Pull Requests (PRs)

Once the student pushes new work:

1. Open Pull Requests on GitHub.
2. Click New Pull Request.
3. Set:
  - Base branch: develop
  - Compare branch: develop-studentGITHUBNAME
4. Review changes and comment as needed.
5. Merge when ready.

PR help: https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests

# 🎓 Student Workflow

## Step 1 — Clone the Repository

### Using GitHub Desktop

1. Open GitHub Desktop.
2. File → Clone Repository.
3. Select the repository [WWM-R-data-workflow](https://github.com/WWM-housinggame-data-analysis/WWM-R-data-workflow) from your GitHub account.
4. Clone to your computer.