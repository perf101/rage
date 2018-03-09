drop table measurements;
drop table tc_config;
drop table machines;
drop table jobs;
drop table builds;
drop table soms;
drop table test_cases;
drop table tiny_urls;
drop table briefs;

create table briefs (
  brief_id serial,
  brief_desc varchar(512) not null,
  brief_params varchar not null,

  primary key (brief_id)
);
grant select on briefs to "www-data";

create table builds (
  build_id serial,
  product varchar(128) not null,
  branch varchar(128) not null,
  build_number integer not null,
  build_tag varchar(128) null,

  primary key (build_id),
  constraint builds_unique_keys unique (product, branch, build_number, build_tag)
);
grant select on builds to "www-data";

create table jobs (
  job_id integer not null,
  build_id integer not null,
  job_cmd text null,

  primary key (job_id),
  foreign key (build_id) references builds(build_id)
);
grant select on jobs to "www-data";

create table machines (
  machine_id serial,
  machine_name varchar(64) not null,
  machine_type varchar(256) not null,
  cpu_model varchar(128) not null,
  number_of_cpus integer not null,

  primary key (machine_id),
  constraint machine_unique_key unique (machine_name)
);
grant select on machines to "www-data";

create table tbljobblacklist (
  jobid integer not null,
  reason varchar(256),

  primary key (jobid)
);

create table tbljobblacklistforsom (
  jobid integer not null,
  somid integer not null,
  reason varchar(256),

  primary key (jobid, somid)
);

create table tblmachineinfo (
  name varchar(32) not null,
  machinetype varchar(256),
  cpumodel varchar(256),
  numcpus integer,

  primary key (name)
);

create table test_cases (
  tc_fqn varchar(64) not null,
  description text null,

  primary key (tc_fqn)
);
grant select on test_cases to "www-data";

create table tc_config (
  job_id integer not null,
  tc_fqn varchar(64) not null,
  tc_config_id integer not null,
  machine_id integer not null,

  dom0_memory_static_max integer not null,
  dom0_memory_target integer null,
  cc_restrictions boolean not null,
  redo_log boolean not null,
  network_backend varchar(32) not null,
  option_clone_on_boot boolean not null,
  force_non_debug_xen boolean not null,
  xenrt_pq_name varchar(64) not null,
  xenrt_version varchar(64) not null,
  xenrt_internal_version varchar(64) not null,
  xenrt_pq_version varchar(64) not null,
  xen_cmdline varchar(128) not null,
  kernel_cmdline varchar(128) not null,
  cpufreq_governor varchar(32) not null,
  dom0_vcpus integer not null,
  host_pcpus integer not null,
  host_type varchar(16) not null,

  foreign key (job_id) references jobs(job_id),
  foreign key (tc_fqn) references test_cases(tc_fqn),
  /* (Cannot reference tc_config_id, since table is variable.) */
  foreign key (machine_id) references machines(machine_id),
  constraint tc_config_unique_key unique
    (job_id, tc_fqn, tc_config_id, machine_id)
);
grant select on tc_config to "www-data";

create table tiny_urls (
  key serial,
  url text not null,

  primary key (key)
);
grant all on tiny_urls to "www-data";
grant all on tiny_urls_key_seq to "www-data";

create table soms (
  som_id integer not null,
  som_name varchar(128) not null,
  tc_fqn varchar(64) not null,
  more_is_better boolean null,
  units varchar(32) null,
  positive boolean not null default true,

  primary key (som_id),
  unique (som_name),
  foreign key (tc_fqn) references test_cases(tc_fqn)
);
grant select on soms to "www-data";

CREATE TABLE soms_jobs (
  id serial,
  som_id integer NOT NULL,
  job_id integer NOT NULL,

  PRIMARY KEY (id),
  CONSTRAINT soms_jobs_unique_keys UNIQUE (som_id, job_id),
  foreign key (som_id) references soms(som_id),
  foreign key (job_id) references jobs(job_id)
);
grant select on soms_jobs to "www-data";
CREATE INDEX soms_jobs_job_id_index ON soms_jobs USING btree (job_id);
CREATE INDEX soms_jobs_som_id_index ON soms_jobs USING btree (som_id);

create table measurements_2 (
  /* Measurement context. */
  som_job_id integer not null,
  tc_config_id integer not null,
  som_config_id integer null,

  /* Measurement. */
  result_id integer not null,
  result double precision not null,

  /* Constraints. */
  constraint measurements_unique_keys unique
    (som_job_id, tc_config_id, som_config_id, result_id),
  foreign key (som_job_id) references soms_jobs(id),
  /* (Cannot reference tc_config_id, since table is variable.) */
  /* (Cannot reference som_config_id, since table is variable.) */
);
grant select on measurements_2 to "www-data";
create index measurements_som_config_id_index on measurements_2 using btree (som_config_id);
create index measurements_som_job_id_index on measurements_2 using btree (som_job_id);
create index measurements_tc_config_id_index on measurements_2 using btree (tc_config_id);

---create materialized view measurements_distinct as select distinct measurements.som_id, measurements.job_id from measurements order by measurements.som_id, measurements.job_id;
---grant select on measurements_distinct to "www-data";
---create index measurements_distinct_job_id_som_id on measurements_distinct using btree (job_id, som_id);
---create index measurements_distinct_som_id_job_id on measurements_distinct using btree (som_id, job_id);

create table tblRacktablesNameMapping (
  strourname varchar(64) not null,
  strracktablesname varchar(64),

  primary key (strourname)
);
