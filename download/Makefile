# DOING SOMETHING WRONG
# 
INFILES = $(wildcard doc/*.3.man)
OUTFILES = $(patsubst doc/%.man,tmp/man/man3/%.gz,$(INFILES))
CMD = txt2man
.SUFFIXES: $(SUFFIXES) .man .gz

all: $(OUTFILES)
        echo made $@ : $^
	cd tmp/man
	mandb -c `pwd`
gz.man:
	@echo rule1
	$(CMD) $< |gzip - > ${@}
%.gz: %.man
	@echo rule
	$(CMD) $< |gzip - > ${@}

$(OUTFILES): $(INFILES)
#      For the target lib(member.o) and the .s2.a rule, the internal macros
#      are defined as follows:
#	@echo "$<  member.s2"
#	@echo "$*  member"
#	@echo "$@  lib"
#	@echo "$?  member.s2"
#	@echo "$%  member.o"
#      When the $?
#      macro contains more than one prerequisite file name, the $(?D) and
#      $(?F) (or ${?D} and ${?F}) macros expand to a list of directory name
#      parts and file name parts respectively.
#	@echo "A ${?D}"
#	@echo "B ${?F}"
#	@echo "C $(?D)"
#	@echo "D $(?F)"
#	@echo "E INFILES $(INFILES)"
#	@echo "F OUTFILES $(OUTFILES)"
# 
#	@printf " lt:$<\n star:$*\n at:$@\n question:$?\n amp:$&\n percent:$%\n"
#	$(CMD) $< |gzip - > ${@}
	$(CMD) doc/`basename ${@} .gz`.man |gzip - > ${@}

#-------------------------------------------------------------------------------
