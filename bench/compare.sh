#!/bin/zsh

tests=$(ls *.scm)

crvm() {
    rm -rf rvm
    rm -rf rvm1
    rm -rf rvm2
    rm -rf rvm3
}

rvm() {
    crvm
    pushd ../src/host/c
    make clean >> /dev/null
    make >> /dev/null
    cp rVM ../../../bench/rvm
    cp rVM1 ../../../bench/rvm1
    cp rVM2 ../../../bench/rvm2
    cp rVM3 ../../../bench/rvm3
    popd
}

cchicken() {
    rm -rf chicken.tar.gz
    rm -rf chicken-5.2.0
}

chicken() {
    cchicken

    wget https://code.call-cc.org/releases/5.2.0/chicken-5.2.0.tar.gz -O chicken.tar.gz > /dev/null 2>&1
    tar -xvf chicken.tar.gz > /dev/null 2>&1

    pushd chicken-5.2.0
    make PLATFORM=linux > /dev/null 2>&1
    cp csi ..
    cp libchicken.so ..
    popd

    ln -s libchicken.so libchicken.so.11
}

cqscheme() {
    rm -rf qscheme
    rm -rf qscheme.tar.gz
}

qscheme() {
    cqscheme
    wget https://www.sof.ch/dan/qscheme/files/qscheme-0.5.1.tar.gz -O qscheme.tar.gz
    tar -xvf qscheme.tar.gz
    cp -r qscheme-0.5.1 qscheme

    pushd qscheme
    ./configure
    make
    popd 
}

ctinyscheme() {
    rm -rf tinyscheme
    rm -rf tinyscheme.tar.gz
    rm -rf tinyscheme-1.41
}

tinyscheme() {
    ctinyscheme
    wget https://pilotfiber.dl.sourceforge.net/project/tinyscheme/tinyscheme/tinyscheme-1.41/tinyscheme-1.41.tar.gz -O tinyscheme.tar.gz > /dev/null 2>&1
    tar -xvf tinyscheme.tar.gz > /dev/null 2>&1

    pushd tinyscheme-1.41
    make clean > /dev/null 2>&1
    make > /dev/null 2>&1
    cp scheme ../tinyscheme
    popd

}

cmitscm() {
    rm -rf fmitscm
    rm -rf mitscm
    rm -rf mitscm.zip
}

mitscm() {
    # https://people.csail.mit.edu/jaffer/scm/index.html
    cmitscm
    wget https://groups.csail.mit.edu/mac/ftpdir/users/jaffer/scm.zip -O mitscm.zip > /dev/null 2>&1
    unzip mitscm.zip > /dev/null 2>&1
    mv scm fmitscm > /dev/null 2>&1

    pushd fmitscm
    {
        ./configure --prefix=/usr/local 
        make scmlit
        cp scmlit ../mitscm
    }> /dev/null 2>&1 
popd
}

cbitscm() {
    rm -rf bit-scheme
}

bitscm() {
    cbitscm
    git clone git@github.com:melvinzhang/bit-scheme.git bit-scheme > /dev/null 2>&1 
    pushd bit-scheme 
    sed -i 's/csi/gsi/' Makefile > /dev/null 2>&1 
    popd
}


smallspace() {
    echo
    echo
}

space() {
    smallspace
    smallspace
}

cpico() {
    rm -rf fpicobit
    rm -rf picobit
    rm -rf picobit-vm
}

picobit() {
    cpico
    git clone git@github.com:SamuelYvon/picobit.git fpicobit > /dev/null 2>&1
    pushd fpicobit

    make clean > /dev/null 2>&1
    make > /dev/null 2>&1

    cp picobit ..
    cp picobit-vm ..

    popd 

}

cminischeme() {
    rm -rf minischeme.tar.gz
    rm -rf minischeme
    rm -rf fminischeme
}

minischeme() {
    wget ftp://ftp.cs.indiana.edu/pub/scheme-repository/imp/minischeme.tar.gz -O minischeme.tar.gz > /dev/null 2>&1
    tar xvf minischeme.tar.gz > /dev/null 2>&1
    mv minischeme fminischeme > /dev/null 2>&1
    pushd fminischeme > /dev/null 2>&1
    mv makefile Makefile > /dev/null 2>&1
    make > /dev/null 2>&1
    cp miniscm ../minischeme
    popd
}

csiod() {
    rm -rf siod
    rm -rf libsiod
    rm -rf fsiod
}

siod() {
    csiod
    mkdir fsiod > /dev/null 2>&1 
    pushd fsiod > /dev/null 2>&1 
    wget http://people.delphiforums.com/gjc/siod.tgz -O siod.tgz > /dev/null 2>&1 
    tar xvf siod.tgz > /dev/null 2>&1 
    sed -i 's/-Xlinker $(LIBSIODDIR)/-Xlinker \./' makefile > /dev/null 2>&1 
    make linux > /dev/null 2>&1 

    cp siod .. > /dev/null 2>&1 
    cp libsiod.so .. > /dev/null 2>&1 

    popd > /dev/null 2>&1 
}

cchibi() {
    rm -rf fchibi
    rm -f chibi
    rm -f lib
    rm -f chibi.tar.gz
}

chibi() {
    cchibi
    wget http://synthcode.com/scheme/chibi/chibi-scheme-0.10.0.tgz -O chibi.tar.gz  > /dev/null 2>&1 
    tar xvf chibi.tar.gz > /dev/null 2>&1 
    mv chibi-scheme-0.10.0 fchibi > /dev/null 2>&1 
    
    pushd fchibi > /dev/null 2>&1 
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/$(pwd)/  > /dev/null 2>&1 
    make clean > /dev/null 2>&1 
    make -j4 > /dev/null 2>&1 
    popd > /dev/null 2>&1 

    ln -sf ./fchibi/chibi-scheme chibi > /dev/null 2>&1 
    ln -sf ./fchibi/lib lib > /dev/null 2>&1 
}

clean() {
    rm *.zip
    rm *.csv
    cpico
    crvm
    csiod
    cchicken
    cqscheme
    ctinyscheme
    cminischeme
    cmitscm
    cbitscm
}

header() {
    space
    echo "=============================================="
    echo "Testing $exe"
    echo "=============================================="
}

run() {
    exe="$1"
    header "$exe"


    benches=()
    for test in $(echo "$tests")
    do
        if [[ -f "./$exe" ]]; then
            benches=(${benches[@]} "cat $test | ./$exe")
        else
            benches=(${benches[@]} "cat $test | $exe")
        fi

    done

    hyperfine --min-runs 2 --max-runs 2 -i --export-csv "bench-$exe.csv" ${benches[@]}
}

runbit() {
    exe="bit"
    header "$exe"

    benches=()
    {
    for test in $(echo "$tests")
    do
        filename="${test%.*}"
        cp "$test" bit-scheme/"$test"
        pushd bit-scheme
        make "$filename.c"
        make "$filename"
        popd

        benches=(${benches[@]} "echo $test && ./$filename")
    done
    } > /dev/null 2>&1

    pushd bit-scheme
    hyperfine --min-runs 2 --max-runs 2 -i --export-csv "bench-$exe.csv" ${benches[@]}
    cp "bench-$exe.csv" ..
    popd

}

runpico() {
    exe="pico"
    header "$exe"

    benches=()
    {
    for test in $(echo "$tests")
    do
        filename="${test%.*}"
        cp "$test" fpicobit/"$test"
        pushd fpicobit
        ./picobit "$test"
        popd

        benches=(${benches[@]} "echo $test && ./picobit-vm ./$filename.hex")
    done
    } > /dev/null 2>&1

    pushd fpicobit
    hyperfine --min-runs 2 --max-runs 2 -i --export-csv "bench-$exe.csv" ${benches[@]}
    cp "bench-$exe.csv" ..
    popd
}

runcompiledrvm() {
    ext="$1"
    exe="compiled_rvm$ext"
    header "compiled_rvm"

    benches=()
    {
    for test in $(echo "$tests")
    do
        filename="${test%.*}"
        cp "$test" ../src/"$test"
        pushd ../src
        gsi ./rsc.scm --target c "$test"
        cp "$filename.scm.c" ./host/c/
        pushd ./host/c/
        make "$filename.o$ext"
        popd
        popd

        benches=(${benches[@]} "echo $test && ./$filename.o$ext")
    done
    } > /dev/null 2>&1

    pushd ../src/host/c
    hyperfine --min-runs 2 --max-runs 2 -i --export-csv "bench-$exe.csv" ${benches[@]}
    cp "bench-$exe.csv" ./../../../bench/
    popd
}

if [[ "$1" == "--clean" ]]; then
    clean
    exit 0
fi

echo "== Preparing Schemes =="
rvm
tinyscheme
bitscm
mitscm
picobit
minischeme
siod
chibi
echo "==       READY       =="
run rvm
run rvm3
run minischeme
run bit.sh
runbit
run pico.sh
runpico
run rvm
run mitscm
run tinyscheme
run siod
run chibi
run crvm.sh
runcompiledrvm
runcompiledrvm 3
