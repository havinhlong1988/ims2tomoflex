SRCDIR = src
BAKDIR = /backup_codes/
EXE = ims2tomoflex

.PHONY: run clean backup
run:
	for d in $(SRCDIR); do (cd $$d; echo $$d; make); done
	./$(EXE) < input
clean:
	rm -f $(EXE)
	for d in $(SRCDIR); do (cd $$d; echo $$d; make $@); done

backup:
	cp -ru src inc tomo.inp Makefile README $(BAKDIR) 
