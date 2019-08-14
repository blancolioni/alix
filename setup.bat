@del/q/f/s bin > nul:
@rmdir bin
@del/q/f/s obj > nul:
@rmdir obj
@del/q/f/s dependencies  > nul:
@rmdir dependencies
mkdir bin
mkdir obj
gprbuild setup.gpr
bin\setup_driver windows
