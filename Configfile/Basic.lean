/-
  iniparser.lean
  A library for working with INI configuration files
-/

structure IniSection where
  name : String
  values : List (String × String)
  deriving Repr, BEq

structure IniConfig where
  sections : List IniSection
  deriving Repr, BEq

/-- Convert IniConfig to a string representation -/
def IniConfig.toString (config : IniConfig) : String := Id.run do
  let mut result := ""
  for section' in config.sections do
    result := result ++ s!"[{section'.name}]\n"
    for (key, value) in section'.values do
      result := result ++ s!"{key} = {value}\n"
    result := result ++ "\n"
  result

namespace IniConfiguration

/-- Represents errors that can occur during INI parsing -/
inductive ParseError
  | invalidLine (lineNum : Nat) (line : String)
  | duplicateSection (name : String)
  | malformedSection (lineNum : Nat) (line : String)
  deriving Repr, BEq

/-- Convert ParseError to a string representation -/
def ParseError.toString : ParseError → String
  | invalidLine lineNum line => s!"Invalid line {lineNum}: {line}"
  | duplicateSection name => s!"Duplicate section: {name}"
  | malformedSection lineNum line => s!"Malformed section at line {lineNum}: {line}"

instance : ToString ParseError where
  toString := ParseError.toString

/-- Trim whitespace and comments from a line -/
def cleanLine (line : String) : String :=
  let noComment := (line.splitOn "#").head!
  let noComment := (noComment.splitOn ";").head!
  noComment.trim

/-- Check if a line is empty or only contains whitespace/comments -/
def isEmptyLine (line : String) : Bool :=
  cleanLine line == ""

/-- Parse a section header line "[section]" -/
def parseSectionHeader? (line : String) : Option String := do
  let line := cleanLine line
  guard $ line.startsWith "["
  guard $ line.endsWith "]"
  some $ line.drop 1 |>.dropRight 1 |>.trim

/-- Parse a key-value line "key = value" -/
def parseKeyValue? (line : String) : Option (String × String) := do
  let line := cleanLine line
  let parts := line.splitOn "="
  guard $ parts.length == 2
  let key := parts[0]!.trim
  let value := parts[1]!.trim
  guard $ key != ""
  some (key, value)

/-- Parse an INI file content into an IniConfig -/
partial def parse (content : String) : Except ParseError IniConfig := do
  let lines := content.splitOn "\n"
  let mut currentSection : Option String := none
  let mut sections : List IniSection := []
  let mut currentValues : List (String × String) := []

  for (lineNum, line) in lines.enum do
    let cleanedLine := cleanLine line
    if isEmptyLine line then
      continue
    else if let some sectionName := parseSectionHeader? cleanedLine then
      -- Save previous section if it exists
      if let some secName := currentSection then
        sections := sections ++ [{ name := secName, values := currentValues }]
      currentSection := some sectionName
      currentValues := []
    else if let some (key, value) := parseKeyValue? cleanedLine then
      if currentSection.isNone then
        -- Create default section if none exists
        currentSection := some "default"
      currentValues := currentValues ++ [(key, value)]
    else
      Except.error <| ParseError.invalidLine lineNum line

  -- Add the last section
  if let some secName := currentSection then
    sections := sections ++ [{ name := secName, values := currentValues }]

  Except.ok ({ sections := sections }: IniConfig)

/-- Get a value from a specific section -/
def getValue? (config : IniConfig) (section' : String) (key : String) : Option String := do
  let section' ← config.sections.find? (·.name == section')
  let (_, value) ← section'.values.find? (·.1 == key)
  some value

/-- Get all key-value pairs in a section -/
def getSection? (config : IniConfig) (section' : String) : Option (List (String × String)) := do
  let section' ← config.sections.find? (·.name == section')
  some section'.values


/-- Write IniConfig to a file -/
def writeFile (config : IniConfig) (filename : String) : IO Unit := do
  IO.FS.writeFile filename config.toString

/-- Read and parse an INI file -/
def readFile (filename : String) : IO (Except ParseError IniConfig) := do
  let content ← IO.FS.readFile filename
  return parse content

end IniConfiguration

/-- Example usage -/
def exampleProgram : IO Unit := open IniConfiguration in do
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
    IO.println $ "Parsed config:"
    IO.println $ config.toString

    -- Access values
    match (getValue? config) "database" "host" with
    | some host => IO.println s!"Database host: {host}"
    | none => IO.println "Host not found"

  | .error err => IO.println s!"Error parsing config: {err}"
