
# hdt_wrapper.sh
# ---------------
# A simple wrapper script around hdt. In most cases, it just passes
# the arguments straight through, but we want to enable a 'go' command, 
# which is going to 'cd' into the projects root directory.
# Since we can't change the directory of a parent process, we need to source this
# within the active shell:

case "$1" in 
    go)
        echo "GOTO'ing"
        tgtdir=$(hdt getrootdir)
        echo "Switching into : $tgtdir"
        cd $tgtdir
        ;;
    *)
        echo "Std Cmd'ing"
        echo Cmd:  hdt "$@"
        hdt "$@"
        ;;
esac
