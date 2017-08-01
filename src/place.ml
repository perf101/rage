open! Core.Std

type t =
  | CreateTinyUrl
  | Default
  | RedirectTinyUrl
  | SomData
  | SomPage
  | Soms
  | SomsByTc
  | StdAxes
  | Brief
  | ImportPage
  | ImportJobs

let of_string = function
  | "create_tiny_url" -> CreateTinyUrl
  | "som" -> SomPage
  | "soms" -> Soms
  | "soms_by_tc" -> SomsByTc
  | "som_data" -> SomData
  | "std_axes" -> StdAxes
  | "brief" -> Brief
  | "import_page" -> ImportPage
  | "import_jobs" -> ImportJobs
  | p -> failwith ("place_of_string: " ^ p)

let string_of = function
  | CreateTinyUrl -> "CreateTinyUrl"
  | Default -> "Default"
  | RedirectTinyUrl -> "RedirectTinyUrl"
  | SomData -> "SomData"
  | SomPage -> "SomPage"
  | Soms -> "Soms"
  | SomsByTc -> "SomsByTc"
  | StdAxes -> "StdAxes"
  | Brief -> "brief"
  | ImportPage -> "import_page"
  | ImportJobs -> "import_jobs"
