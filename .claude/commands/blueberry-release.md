---
allowed-tools: Bash(git:*), Bash(gh:*), Bash(makepkg:*), Read, Edit
description: Tag, release, update AUR for blueberry-monad
---

# Blueberry Release Workflow

## 1. Extract Version
Read `package.yaml` and extract version from line starting with `version:`.

## 2. Check Tag Doesn't Exist
Run `git tag -l "v<VERSION>"` and `git ls-remote --tags origin "v<VERSION>"`.
If tag exists locally or remotely, STOP with error.

## 3. Create and Push Tag
- `git tag v<VERSION>`
- `git push origin v<VERSION>`

## 4. Wait for Release CI
Get the workflow run ID and wait:
- `gh run list --workflow=release.yml --limit=1 --json databaseId -q '.[0].databaseId'`
- `gh run watch <ID> --exit-status`

If CI fails, STOP and report error.

## 5. Update AUR Repository
Change to `~/Development/Aur/bbdata-aur`:
- `git pull --rebase`
- Update `blueberry-monad/PKGBUILD`: set `pkgver=<VERSION>` and `pkgrel=1`
- Regenerate srcinfo: `cd blueberry-monad && makepkg --printsrcinfo > .SRCINFO`

## 6. Commit and Push AUR
- `git add blueberry-monad/`
- `git commit -m "blueberry-monad: <VERSION>"`
- `git push origin main`

## 7. Report Success
Confirm release complete with version and links.
