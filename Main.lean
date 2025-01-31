import Configfile

/-- Example usage -/
def exampleProgram : IO Unit := do
  let sampleConfig := "
    [database]
    host = localhost
    port = 5432
    user = admin

    [server]
    port = 8080
    debug = true
    "

  match parse sampleConfig with
  | .ok config => do
    IO.println "Parsed config:"
    IO.println config.toString

    -- Access values
    match config.getValue? "database" "host" with
    | some host => IO.println s!"Database host: {host}"
    | none => IO.println "Host not found"

  | .error err => IO.println s!"Error parsing config: {err}"


  let mut sc: SectionedConfig := Inhabited.default
  sc := sc.addSection "basic"
  sc := sc.addValue "basic" "team" "none"
  sc := sc.addValue "basic" "chutney" "excessive"
  sc := sc.addValue "advanced" "hogs" "34"

  IO.println ""
  IO.println "Other Parsed Config:"
  IO.println sc.toString


def main : IO Unit := do
  exampleProgram
