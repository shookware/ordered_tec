
EXE = ordered_tec
FC = ifort
IDIR =
CFLAGS = -fpic -mod $(MODS_DIR) $(IDIR)
LFLAGS =
LIBS =
SHARED_EXT = .dylib
#AR = ar
#ARFLAG = -cuvs
#RANLIB = ranlib


SRC_DIR_BeFoR64 = ./src/BeFoR64/
SRC_DIR_PENF = ./src/PENF/
SRC_DIR_MAIN = ./src/
SRC_DIR_StringiFor = ./src/StringiFor/
TESTS_DIR = ./test/
OBJS_DIR = ./obj/
MODS_DIR = ./mod/
EXE_DIR = ./
LIB_DIR = ./lib/

SRCS_MAIN = $(notdir $(wildcard $(SRC_DIR_MAIN)*.f90))
SRCS_BeFoR64 = $(notdir $(wildcard $(SRC_DIR_BeFoR64)*.F90))
SRCS_PENF = $(notdir $(wildcard $(SRC_DIR_PENF)*.F90))
SRCS_StringiFor = $(notdir $(wildcard $(SRC_DIR_StringiFor)*.F90))


OBJS_MAIN = $(SRCS_MAIN:.f90=.o)
OBJS_BeFoR64 = $(SRCS_BeFoR64:%.F90=%.o)
OBJS_PENF = $(SRCS_PENF:.F90=.o)
OBJS_StringiFor = $(SRCS_StringiFor:.F90=.o)

VPATH = $(SRC_DIR_BeFoR64):$(OBJS_DIR):$(SRC_DIR_PENF):$(OBJS_DIR):$(SRC_DIR_MAIN):$(OBJS_DIR):$(SRC_DIR_StringiFor):$(OBJS_DIR)
OBJS = $(addprefix $(OBJS_DIR), $(OBJS_BeFoR64) $(OBJS_PENF) $(OBJS_MAIN) $(OBJS_StringiFor))

EX := $(foreach n, 1, ex$(n))
EX_OBJS := $(EX:=.o)

all : $(EXE)

tec_lib: libtec_file$(SHARED_EXT)
#	@$(RANLIB) $(LIB_DIR)libtec_file.a
	@mkdir -p $(LIB_DIR)mod
	@cp -r $(MODS_DIR) $(LIB_DIR)mod
libtec_file$(SHARED_EXT): $(OBJS_BeFoR64) $(OBJS_PENF) $(OBJS_MAIN) $(OBJS_StringiFor)
	@mkdir -p $(LIB_DIR)
	@$(FC) $(CFLAGS) -shared -o $(LIB_DIR)libtec_file$(SHARED_EXT) $(OBJS)

$(EXE) : $(OBJS_BeFoR64) $(OBJS_PENF) $(OBJS_MAIN) $(OBJS_StringiFor)
	@mkdir -p $(EXE_DIR)
	@mkdir -p $(MODS_DIR)
	$(FC) -o $(EXE_DIR)$(EXE) $(OBJS) $(LFLAGS) $(LIBS)

test : $(EX)

$(EX): tec_lib $(EX_OBJS)
	$(FC) -I $(MODS_DIR) -ltec_file -L $(LIB_DIR) -o $(TESTS_DIR)$@/$@ $(OBJS_DIR)$@.o
$(EX_OBJS):
	$(FC) $(CFLAGS) -c $(TESTS_DIR)$(@:.o=)/$(@:.o=.f90) -o $(OBJS_DIR)$@

$(OBJS_BeFoR64):
	@mkdir -p $(OBJS_DIR)
	@mkdir -p $(MODS_DIR)
	$(FC) $(CFLAGS) -c $(SRC_DIR_BeFoR64)$(@:.o=.F90) -o $(OBJS_DIR)$@

$(OBJS_PENF):
	@mkdir -p $(OBJS_DIR)
	@mkdir -p $(MODS_DIR)
	$(FC) $(CFLAGS) -c $(SRC_DIR_PENF)$(@:.o=.F90) -o $(OBJS_DIR)$@

$(OBJS_MAIN):
	@mkdir -p $(OBJS_DIR)
	@mkdir -p $(MODS_DIR)
	$(FC) $(CFLAGS) -c $(SRC_DIR_MAIN)$(@:.o=.f90) -o $(OBJS_DIR)$@

$(OBJS_StringiFor):
	@mkdir -p $(OBJS_DIR)
	@mkdir -p $(MODS_DIR)
	$(FC) $(CFLAGS) -c $(SRC_DIR_StringiFor)$(@:.o=.F90) -o $(OBJS_DIR)$@

.PHONY: clean
clean :
	rm -fr $(OBJS_DIR)
	rm -f  $(EXE_DIR)$(EXE)
	rm -fr $(MODS_DIR)

# Dependencies of files
befor64.o: \
    befor64.F90 \
    befor64_pack_data_m.o \
    penf.o
penf_global_parameters_variables.o: \
    penf_global_parameters_variables.F90
ordered_tec.o: \
    ordered_tec.f90 \
    penf.o \
    stringifor.o
stringifor.o: \
    stringifor.F90 \
    penf.o \
    stringifor_string_t.o
stringifor_string_t.o: \
    stringifor_string_t.F90 \
    befor64.o \
    penf.o
penf_b_size.o: \
    penf_b_size.F90 \
    penf_global_parameters_variables.o
befor64_pack_data_m.o: \
    befor64_pack_data_m.F90 \
    penf.o
penf_stringify.o: \
    penf_stringify.F90 \
    penf_b_size.o \
    penf_global_parameters_variables.o
penf.o: \
    penf.F90 \
    penf_b_size.o \
    penf_global_parameters_variables.o \
    penf_stringify.o
