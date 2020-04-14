# FC = pgf90
FC = gfortran

# Flags for pgf90
# FLAGS = -O3 -pc 64 -Kieee -Mdalign -Mextend -Mnoframe -byteswapio
# FLAGS = -g -pc 64 -Kieee -Mdalign -Mextend -Mnoframe -byteswapio

# Flags for gfortran
FLAGS = -g -Wall -Wno-tabs -fconvert=big-endian

TARGET = MID_RAMPA

MODULES = \
class_uam_iv.o 
PROGRAMS = \
MID_RAMPA.o

OBJECTS = $(MODULES) $(PROGRAMS)

MID_RAMPA: $(OBJECTS)
	$(FC) $^ -o $@ $(FLAGS)

%.mod: %.f90
	$(FC) -c $^ $(FLAGS)

%.o: %.f90
	$(FC) -c $^ $(FLAGS)

.PHONY: clean

clean:
	rm -f $(OBJECTS) *.mod