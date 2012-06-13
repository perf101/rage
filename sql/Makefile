SHELL=bash
CONFIG=/usr/groups/perfeng/rage/config
PERF_DB=$(shell grep "^perf_db=" $(CONFIG) | awk -F '=' '{print $$2}')
PERF_HOST=$(shell grep "^perf_host=" $(CONFIG) | awk -F '=' '{print $$2}')
PERF_USER=$(shell grep "^perf_user=" $(CONFIG) | awk -F '=' '{print $$2}')
PERF_PASS=$(shell grep "^perf_pass=" $(CONFIG) | awk -F '=' '{print $$2}')
BACKUP_DIR=$(shell grep "^rage_db_backup_dir=" $(CONFIG) | awk -F '=' '{print $$2}')
BACKUP_FILE="${BACKUP_DIR}/${shell date +%s}.sql"

.PHONY: default
default:
	@echo "Type 'make reset' to clear the database."

.PHONY: backup
backup:
	sudo su postgres -c "pg_dump ${PERF_DB}" > ${BACKUP_FILE}

.PHONY: reset
reset: schema.sql
	PGPASSWORD=$(PERF_PASS) psql -h $(PERF_HOST) -U $(PERF_USER) $(PERF_DB) < $<

.PHONY: clean
clean:
	@rm -f *.{log,sql}
