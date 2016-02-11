oe () {
    if [ -z "$1" ]; then
        fu oe help
    elif [ "$1" == "cd" ]; then
        local where=$2
        local recipe=$3
        if [ -z "$where" ]; then
            cd $BUILDDIR
            return 0
        fi
        local dir
        case "$where" in
            top)
                if [ -z "$BUILDDIR" ]; then
                    echo "BUILDDIR is not set" >&2
                    return 1
                else
                    dir=$BUILDDIR
                fi
                ;;
            bh) dir=`fu oe x -s BUILDHISTORY_DIR` ;;
            pkg) dir=`fu oe x -s DEPLOY_DIR` ;;
            wd)
                if [ -z "$3" ]; then
                    echo "Usage: oe cd wd <recipe>" >&2
                    return 1
                fi
                dir=`fu oe x -s WORKDIR $recipe`
                ;;
            *) dir=
        esac
        if [ -z "$dir" ]; then
            echo "Unknown location specifier: $where" >&2
            return 1
        fi
        # oe prints the value quoted, so we remove quotes here
        dir=`echo $dir | sed 's/^"//; s/"$//'`
        cd $dir
    else
        fu oe "$@"
    fi
}
