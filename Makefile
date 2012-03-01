# PREDEFINED VARIABLES
FFLAGS = -c -O3 -Wall -I. -Wno-unused-dummy-argument
FORT = gfortran

# MODULES 
MODULES  = mylibrary param
MODULESF90  = $(MODULES:=.f90)
MODULESO    = $(MODULES:=.o)

#ROUTINES
ROUTINES = boundaries initial_data sources
ROUTINESF90 = $(ROUTINES:=.f90)
ROUTINESO   = $(ROUTINES:=.o)

#MAIN PROGRAM
PROGRAM  = main

#EXECUTABLE
EXEC = exec.out


# LINKING
$(PROGRAM).out: $(MODULES) $(ROUTINES) $(PROGRAM).o
	$(FORT) $(PROGRAM).o $(MODULESO) $(ROUTINESO) -o $(EXEC)
	rm -rf $(MODULESO) $(ROUTINESO) $(PROGRAM).o $(MODULES).mod

# MODULES' RULES
$(MODULES): $(MODULESF90) 
	$(FORT) $(FFLAGS) $@.f90 -o $@.o

# ROUTINES' RULES
$(ROUTINES): $(ROUTINESF90) 
	$(FORT) $(FFLAGS) $@.f90 -o $@.o

# PROGRAM COMPILATION
$(PROGRAM).o: $(PROGRAM).f90 
	$(FORT) $(FFLAGS) $< -o $@

run:
	./$(EXEC)

clean:
	rm -rf *.out *.o *.f90# *.log *.mod *.f90~

all: clean $(PROGRAM).out run


