open Core.Std
open Printf
open Utils

type t =
  | CreateTinyUrl
  | Default
  | RedirectTinyUrl
  | Report
  | ReportClone
  | ReportCreate
  | ReportGeneratorPage
  | ReportDelete
  | ReportPage
  | Reports
  | SomData
  | SomIndex
  | SomPage
  | Soms
  | SomsByTc
  | StdAxes

let of_string = function
  | "create_tiny_url" -> CreateTinyUrl
  | "report" -> Report
  | "report_clone" -> ReportClone
  | "report_create" -> ReportCreate
  | "report_delete" -> ReportDelete
  | "report_generator_page" -> ReportGeneratorPage
  | "report_page" -> ReportPage
  | "reports" -> Reports
  | "som" -> SomPage
  | "soms" -> Soms
  | "soms_by_tc" -> SomsByTc
  | "som_data" -> SomData
  | "som_index" -> SomIndex
  | "std_axes" -> StdAxes
  | p -> failwith ("place_of_string: " ^ p)

let string_of = function
  | CreateTinyUrl -> "CreateTinyUrl"
  | Default -> "Default"
  | RedirectTinyUrl -> "RedirectTinyUrl"
  | Report -> "Report"
  | ReportClone -> "ReportClone"
  | ReportCreate -> "ReportCreate"
  | ReportDelete -> "ReportDelete"
  | ReportGeneratorPage -> "ReportGeneratorPage"
  | ReportPage -> "ReportPage"
  | Reports -> "Reports"
  | SomData -> "SomData"
  | SomIndex -> "SomIndex"
  | SomPage -> "SomPage"
  | Soms -> "Soms"
  | SomsByTc -> "SomsByTc"
  | StdAxes -> "StdAxes"
