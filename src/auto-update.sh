#!/usr/bin/bash

ROOT="$( dirname "${BASH_SOURCE[0]}" )"
year=`date "+%Y"`
hoje=`date +"%Y-%m-%d"`
ontem=`date -d "yesterday" +"%Y-%m-%d"`

print_help(){
    echo -e "\nBaixa dados do MS e atualiza o site.\n"
    echo -e "  Opções:\n"
    echo -e "    -h help"
#    echo -e "    -q quieto"
    echo -e "    -d não faça download dos dados"
    echo -e "    -r não faça atualização do site"
    echo -e "    -f atualize o site mesmo que não haja dados novos\n"
}

# command-line options
OPTIND=1         # Reset in case getopts has been used previously in the shell.
quiet=0
download=1
update=1
force=0
while getopts "drqh" opt; do
    case "$opt" in
    h)
        print_help
        exit 0
        ;;
#    q)  quiet=1
#        ;;
    d)  download=0
        ;;
    r)  update=0
        ;;
    f)  force=1
        ;;
    *)
        print_help
        exit 1
    esac
done
shift $((OPTIND-1))
[ "${1:-}" = "--" ] && shift

if [ $download = 1 ]; then
    python downloader.py
    downloaded=$?
    if [ $downloaded = 0 ]; then
        git diff-index --quiet HEAD -- ../dados
        if [ $? != 0 ]; then
            git add ../dados/BrasilCov19.csv ../dados/EstadosCov19.csv ../dados/brutos/
            git commit -m "[auto] Dados de hoje."
        else
            downloaded=1
        fi
    fi
fi

if [[ $force = 1 || ( $update = 1  && ( $downloaded = 0 || -z $downloaded )) ]]; then
    R -q --no-save < update.R
    if [ $? = 0 ]; then
        pushd ..
        git add outputs/prev.5d.csv outputs/tempos.duplicacao.csv outputs/*.prev.5d.csv outputs/*.tempos.duplicacao.csv *.html
        popd
        git commit -m "[auto] Novas projeções."
        git push
    else
        echo "** Erro na atualização do site. **"
        exit 1
    fi
else
    echo "** Nenhum dado novo, nenhuma projeção realizada. **"
fi
