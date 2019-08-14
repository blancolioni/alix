# alix
A package manager for Ada

## Using alix

### Install an alix package

```
alix install <package-name>
```

### Creating a project from a source tree

In the directory containing the alix file, type

```
alix configure
```

This checks that all dependencies are installed, and creates a .gpr

## Installing alix

Clone the alix git repository, then run the setup script.

On windows:

```
setup
```

On Unix:

```
sh ./setup.sh
```

You must provide two paths (where should packages be installed, and where should executables be installed).

Make sure that the first path is in GPR_PROJECT_PATH, and the second is in PATH.

