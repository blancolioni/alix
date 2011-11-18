#!/bin/sh
cd {Temp_Folder}
rm -rf {Project}
mkdir -p {Project}
cd {Project}
hg clone {Server} {Version}
