oe_sh_usage() {
    cat <<EOF
cd <place>
  Change directory to <place>.  The available <place>s are:

    top
      Change to the directory pointed by the by the BUILDDIR variable.
      top is implicitly assumed as <place> if cd is given none.

    buildhistory
      Short: bh.  Change to the directory pointed by the BUILDHISTORY_DIR
      variable.

    deploy
      Short: dd.  Change to the directory pointed by the DEPLOY_DIR
      variable.

    work <recipe>
      Short: wd.  Change to the directory pointed by the WORKDIR
      variable for recipe <recipe>.
EOF
}

oe () {
    if [ -z "$1" ]; then
        fu oe
        oe_sh_usage >&2
        return 1
    elif [ "$1" = "help" ] || [ "$1" = "-h" ] || [ "$1" = "-help" ] || [ "$1" = "--help" ]; then
        fu oe help
        oe_sh_usage
        return 0
    fi

    if [ -z "$BUILDDIR" ]; then
        echo "BUILDDIR is not set.  Aborting." >&2
        return 1
    fi

    if [ "$1" == "cd" ]; then
        local where=$2
        local recipe=$3
        local dir
        case "$where" in
            ""|top) dir=$BUILDDIR ;;
            bh|buildhistory) dir=`fu oe x -s BUILDHISTORY_DIR` ;;
            dd|deploy) dir=`fu oe x -s DEPLOY_DIR` ;;
            wd|work)
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
