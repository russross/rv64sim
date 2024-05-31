.SUFFIXES:

ASFLAGS=-g --warn --fatal-warnings -march=rv64im
LDFLAGS=--fatal-warnings --no-relax
ifneq ("$(shell which riscv64-linux-gnu-as)", "")
	PREFIX=riscv64-linux-gnu-
else ifneq ("$(shell which riscv64-unknown-elf-as)", "")
	PREFIX=riscv64-unknown-elf-
else ifneq ("$(shell which riscv64-alpine-linux-musl-as)", "")
	PREFIX=riscv64-alpine-linux-musl-
else
	PREFIX=unknown
endif
AS=$(PREFIX)as
LD=$(PREFIX)ld
CPP=$(PREFIX)cpp
RUN=qemu-riscv64

ALLOBJECT=$(sort $(patsubst %.s,%.o,$(wildcard *.s))) $(sort $(patsubst %.S,%.o,$(wildcard *.S)))
START=$(filter start.o, $(ALLOBJECT))
AOUTOBJECT=$(START) $(filter-out $(START), $(ALLOBJECT))
HEADERS=$(wildcard *.h)

all:	a.out

%.s:	%.S $(HEADERS)
	$(CPP) -DTEST_INSTRUCTION=test_$(subst .S,,$<) $< -o $@

%.o:	%.s
	$(AS) $(ASFLAGS) $< -o $@

a.out: $(AOUTOBJECT)
	$(LD) $(LDFLAGS) $^ -o $@

clean:
	rm -f *.o $(patsubst %.S,%.s,$(wildcard *.S)) a.out
