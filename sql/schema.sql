drop table report_plot_split_bys;
drop table report_plot_som_configs;
drop table report_plot_tc_configs;
drop table report_plots;
drop table report_builds;
drop table reports;
drop table measurements;
drop table tc_config;
drop table machines;
drop table jobs;
drop table branch_order;
drop table standard_builds;
drop table builds;
drop table soms;
drop table test_cases;
drop table tiny_urls;

drop sequence tiny_urls_key_seq;
create sequence tiny_urls_key_seq;
grant all on tiny_urls_key_seq to "www-data";
create table tiny_urls (
  key integer default nextval('tiny_urls_key_seq'::regclass),
  url text not null,

  primary key (key)
);
grant all on tiny_urls to "www-data";

create table test_cases (
  tc_fqn varchar(64) not null,
  description text null,

  primary key (tc_fqn)
);
grant select on test_cases to "www-data";

create table soms (
  som_id integer not null,
  som_name varchar(128) not null,
  tc_fqn varchar(64) not null,
  more_is_better boolean null,
  units varchar(32) null,

  primary key (som_id),
  unique (som_name),
  foreign key (tc_fqn) references test_cases(tc_fqn)
);
grant select on soms to "www-data";

create table builds (
  build_id serial,
  product varchar(128) not null default 'XenServer',
  branch varchar(128) not null,
  build_number integer not null,
  build_tag varchar(128) null,

  primary key (build_id),
  constraint builds_unique_keys unique (product, branch, build_number, build_tag)
);
grant select on builds to "www-data";

create table standard_builds (
  build_number integer not null,
  build_name varchar(256) not null,

  unique (build_number),
  unique (build_name),
  foreign key (build_number) references builds(build_number)
);
grant select on standard_builds to "www-data";

create table branch_order (
  branch varchar(128) not null,
  seq_number integer not null,

  unique (branch)
);
create index branch_order_seq_number_index on branch_order(seq_number);
grant select on branch_order to "www-data";

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

create table tc_config (
  job_id integer not null,
  tc_fqn varchar(64) not null,
  tc_config_id integer not null,
  machine_id integer not null,
  dom0_memory_static_max integer not null default 752,
  dom0_memory_target integer null,
  cc_restrictions boolean not null default false,

  foreign key (job_id) references jobs(job_id),
  foreign key (tc_fqn) references test_cases(tc_fqn),
  /* (Cannot reference tc_config_id, since table is variable.) */
  foreign key (machine_id) references machines(machine_id),
  constraint tc_config_unique_key unique
    (job_id, tc_fqn, tc_config_id, machine_id)
);
grant select on tc_config to "www-data";

create table measurements (
  /* Measurement context. */
  job_id integer not null,
  tc_config_id integer not null,
  som_id integer not null,
  som_config_id integer null,

  /* Measurement. */
  result_id integer not null,
  result double precision not null,

  /* Constraints. */
  constraint measurements_unique_keys unique
    (job_id, tc_config_id, som_id, som_config_id, result_id),
  foreign key (job_id) references jobs(job_id),
  /* (Cannot reference tc_config_id, since table is variable.) */
  foreign key (som_id) references soms(som_id)
  /* (Cannot reference som_config_id, since table is variable.) */
);
create index measurements on table using hash (som_id);
grant select on measurements to "www-data";

create table reports (
  report_id serial,
  report_desc varchar(256) not null,
  xaxis varchar(128) not null,
  yaxis varchar(128) not null,

  primary key (report_id),
  unique (report_desc)
);
grant select on reports to "www-data";

create table report_builds (
  report_id integer not null,
  build_id integer not null,
  "primary" boolean not null,

  constraint report_builds_unique_key unique (report_id, build_id, "primary"),
  foreign key (report_id) references reports(report_id),
  foreign key (build_id) references builds(build_id)
);
grant select on report_builds to "www-data";

create table report_plots (
  plot_id serial,
  report_id integer not null,
  graph_number integer not null,
  som_id integer not null,

  primary key (plot_id),
  foreign key (report_id) references reports(report_id),
  foreign key (som_id) references soms(som_id)
);
grant select on report_plots to "www-data";

create table report_plot_tc_configs (
  plot_id integer not null,
  tc_config_id integer not null,

  foreign key (plot_id) references report_plots(plot_id)
);
grant select on report_plot_tc_configs to "www-data";

create table report_plot_som_configs (
  plot_id integer not null,
  som_config_id integer not null,

  foreign key (plot_id) references report_plots(plot_id)
);
grant select on report_plot_som_configs to "www-data";

create table report_plot_split_bys (
  plot_id integer not null,
  property varchar(128) not null,

  foreign key (plot_id) references report_plots(plot_id)
);
grant select on report_plot_split_bys to "www-data";
