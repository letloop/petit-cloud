LETLOOP=letloop

all: blake3
	echo 'win'

check: all
	$(LETLOOP) check .

local/lib/libblake3.so:
	rm -rf local/src/blake3
	mkdir -p local/src/blake3
	cd local/src &&	git clone --depth=1 https://github.com/BLAKE3-team/BLAKE3 blake3
	cd local/src/blake3/c/ && gcc -shared -O3 -o libblake3.so blake3.c blake3_dispatch.c blake3_portable.c blake3_sse2_x86-64_unix.S blake3_sse41_x86-64_unix.S blake3_avx2_x86-64_unix.S blake3_avx512_x86-64_unix.S
	mkdir -p local/lib/
	cp local/src/blake3/c/libblake3.so local/lib/

blake3: local/lib/libblake3.so

