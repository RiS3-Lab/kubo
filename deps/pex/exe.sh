exe(){
    opt=${1}
    libgatso=${2}
    bc=${3}
    to=${4}
    $opt -analyze -load=$libgatso -gatlin -gating=cap -ccv=0 -ccf=1 -cct=0 -ccvv=0 -ccfv=0 -cctv=0 -cvf=0 -skipfun=skip.fun -skipvar=skip.var -lsmhook=lsm.hook -prt-good=0 -prt-bad=1 -prt-ign=0 -stats $bc 2>$to 
}

exe ${1} ${2} ${3} ${4}
