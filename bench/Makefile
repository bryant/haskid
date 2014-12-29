ffi: ffi.hs hashidsffi.o hashidsxx/hashids.o
	ghc -O3 -fforce-recomp -i../src $^ -lstdc++

hashidsffi.o: hashidsffi.cpp
	g++ -std=c++11 -O3 -fPIC -c $^ -o $@

hashidsxx/hashids.o:
	make -C hashidsxx

clean:
	rm -f *.o hashidsxx/*.o
