# Contributing Guidelines

This repository supports collaboration between a **supervisor** and **students** developing R analysis code.  
To avoid conflicts and ensure a clear development process, we follow a structured **Git branching workflow**.

In order to facilitate healthy, constructive behavior in an open and inclusive community, we all respect and abide by our [Code of Conduct](./CODE_OF_CONDUCT.md).

Please read these instructions before contributing.

<br>

# 📚 Overview of the Workflow

We maintain the following Git branches:

- **`main`** → stable, production-ready code  
- **`develop`** → integration branch for all ongoing work  
- **`develop-studentGITHUBNAME`** → *one branch per student*, created by the supervisor  

<br>

Workflow summary:

1. Repository contains `main` and `develop`.
2. Supervisor creates `develop-studentGITHUBNAME` rooted in `develop`.
3. Student clones repository, works on their branch, and pushes updates.
4. Supervisor creates a pull request (PR) from the student branch into `develop`.
5. Student continues committing and pushing updates.
6. The cycle repeats until the work is complete.

> **Additional help:** Check [the DCC Guides](https://tu-delft-dcc.github.io/) pages on [Collaboration](https://tu-delft-dcc.github.io/docs/software/development_workflow/collaboration.html) and [Branch Management](https://tu-delft-dcc.github.io/docs/software/development_workflow/branch_management.html) for more information on Git workflows and best practices.

<br>

# 🔧 Tool Setup Instructions

To contribute to this project, you need to set up your local environment. Follow the instructions in [INSTALL.md](INSTALL.md) to get started.

In short, contributors to this project may submit their changes with **GitHub Desktop** (recommended for beginners) or via the **terminal** (Mac Terminal, Windows WSL, or Git Bash).

# 🧑‍🏫 Supervisor Workflow

### Step 1 — Create Student Branch

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

> **Branching help:** https://docs.github.com/en/repositories/configuring-branches-and-merges-in-your-repository/managing-branches-in-your-repository

---

### Step 2 — Create Pull Requests (PRs)

Once the student pushes new work:

1. Open Pull Requests on GitHub.
2. Click New Pull Request.
3. Set:
    - Base branch: develop
    - Compare branch: develop-studentGITHUBNAME
4. Review changes and comment as needed.
5. Merge when ready.

> **PR help:** https://docs.github.com/en/pull-requests/collaborating-with-pull-requests/proposing-changes-to-your-work-with-pull-requests/about-pull-requests

<br>

# 🎓 Student Workflow

### Step 1 — Clone the Repository

#### Using GitHub Desktop

1. Open GitHub Desktop.
2. File → Clone Repository.
3. Select the repository [WWM-R-data-workflow](https://github.com/WWM-housinggame-data-analysis/WWM-R-data-workflow) from your GitHub account.
4. Clone to your computer.

> **Cloning help:** https://docs.github.com/en/desktop/adding-and-cloning-repositories

#### Using Terminal (Mac, WSL, Git Bash) with HTTPS connection

```bash
git clone https://github.com/WWM-housinggame-data-analysis/WWM-R-data-workflow.git
cd WWM-R-data-workflow
```

---

### Step 2 — Switch to Your Assigned Branch

#### GitHub Desktop

Select the branch using the branch selector dropdown:

```
develop-studentGITHUBNAME
```

#### Terminal

```bash
git checkout develop-studentGITHUBNAME
```

### Step 3 — Make Changes in R

Add/Edit R scripts in inside the folder`./WWM-R-data-workflow/scripts`.
Check the [DCC Guides](https://tu-delft-dcc.github.io/docs/data/data_collection/data_conventions.html) for data and code conventions.
Test your code before committing.

---

### Step 4 — Commit and Push Changes

#### GitHub Desktop

1. Stage changes (checkboxes).
2. Write a meaningful commit message.
3. Click Commit to develop-studentGITHUBNAME.
4. Click Push origin.

#### Terminal

```bash
git add .
git commit -m "Describe your change"
git push origin develop-studentGITHUBNAME
```

---

### Step 5 — Continue Work or Respond to Supervisor Pull Requests

If you intend to make further changes in your work, either to make progress or to make changes requested by your supervisor, here is how you, the student, should respond.

#### GitHub Desktop

1. Open GitHub Desktop and make sure you're on your branch. You can select it from the Current Branch dropdown.

```
develop-studentGITHUBNAME
```

2. Pull the latest changes (if the supervisor added comments or edits) by clicking `Repository → Pull`. If GitHub Desktop prompts you to "fetch origin", do that first.

3. For comments, requested changes or other suggestions from your supervisor, go to [PR comments](https://github.com/WWM-housinggame-data-analysis/WWM-R-data-workflow/pulls) in WWM-R-data-workflow.

4. Open your R scripts and implement the necessary fixes or improvements.

5. Commit your changes by:
    - selecting  the changed files (checkboxes)
    - writing a clear commit message (e.g. "Address supervisor comments: fix plotting function")
    - clicking `Commit to develop-studentGITHUBNAME`

6. Push changes to GitHub by clicking `Push origin`.

#### Terminal

1. Make sure your branch is checked out

```bash
git checkout develop-studentGITHUBNAME
```

2. Pull the latest changes from GitHub

```bash
git pull
```

3. For comments, requested changes or other suggestions from your supervisor, go to [PR comments](https://github.com/WWM-housinggame-data-analysis/WWM-R-data-workflow/pulls) in WWM-R-data-workflow.

4. Open your R scripts and implement the necessary fixes or improvements.

5. Commit and push changes

```bash
git add .
git commit -m "Describe your change"
git push origin develop-studentGITHUBNAME
```

**Note**: if a Pull Request is open with your branch as **compare branch**, your new commits are automatically added to the it — no further action needed.

<br>

# 🧹 Code Style Suggestions

- Write clear, concise commit messages.
- Keep commits small and focused.

<br>

# 🙋 Need Help?

- Git fundamentals (Software Carpentry): https://swcarpentry.github.io/git-novice/
- TU Delft DCC GitHub Guide: https://tu-delft-dcc.github.io/GitHub-Guide/
- For repository‑specific questions, consult your supervisor.