// Writes graphql-js's own introspection of the schemas smear's executor is compared against, as the
// goldens `smear/tests/introspection_parity.rs` reads.
//
// Run by hand, from this directory, after a change to a fixture or to the pinned release:
//
//   npm ci
//   node generate.mjs
//
// CI never runs it. The goldens are committed, and each one carries the provenance this script
// recorded: the graphql-js release, the options, and the SHA-256 of the SDL it was answered
// for — which the test checks against the fixture it reads, so an edited fixture with a stale golden
// fails there rather than comparing against an answer for some other schema.
//
// What is generated is `introspectionFromSchema(buildSchema(sdl), OPTIONS)`: graphql-js 17.1.0's
// answer to its own full introspection query, executed against the schema it builds from the same
// SDL smear builds from. `buildSchema` gets no options — in particular not `assumeValidSDL`, so a
// fixture graphql-js would refuse is refused here rather than answered.

import { createHash } from 'node:crypto';
import { mkdirSync, readFileSync, writeFileSync } from 'node:fs';
import { dirname, join } from 'node:path';
import { fileURLToPath } from 'node:url';

import {
  buildSchema,
  getIntrospectionQuery,
  graphqlSync,
  introspectionFromSchema,
  version,
} from 'graphql';

const ROOT = join(dirname(fileURLToPath(import.meta.url)), '..', '..');

/** The release every golden is graphql-js's answer from. `package-lock.json` pins the same. */
const RELEASE = '17.1.0';

/**
 * The eight options of graphql-js 17.1.0's `getIntrospectionQuery`: seven on, and `typeDepth` at
 * its default of 9.
 *
 * Passed explicitly, all eight, rather than left to `introspectionFromSchema`'s own defaults: that
 * function turns six of them on and leaves `descriptions` and `typeDepth` to
 * `getIntrospectionQuery`'s defaults, `true` and 9, and a golden whose flags depended on two layers
 * of defaults would not say what it was generated with. These are the flags of the query smear
 * executes — asserted below against the committed `introspection_query.graphql`, byte for byte.
 *
 * `experimentalDirectiveDeprecation` asks for `__schema.directives(includeDeprecated: true)` and
 * for `__Directive`'s `isDeprecated` and `deprecationReason`, which draft §4.2 declares and smear
 * answers, so a deprecated directive is compared like any other.
 */
const OPTIONS = Object.freeze({
  descriptions: true,
  specifiedByUrl: true,
  directiveIsRepeatable: true,
  schemaDescription: true,
  inputValueDeprecation: true,
  experimentalDirectiveDeprecation: true,
  oneOf: true,
  typeDepth: 9,
});

/** The query smear executes, committed beside the executor's own tests. */
const QUERY = 'smear/tests/fixtures/executables/introspection_query.graphql';

/**
 * The schemas the executor's introspection is checked against elsewhere, by name: the agreement
 * fixture `validator_introspection.rs` holds every construct a schema can in, and the two public
 * schemas it and `proto_introspection.rs` take for volume. And one more, `defaults`, which exists
 * for this differential alone: numbers, enum values and strings spelled where two printers could
 * part, which the `defaultValue` comparison must find equal by spelling, and an input object the
 * two print in their own forms.
 */
const FIXTURES = [
  ['agreement', 'smear/tests/fixtures/schemas/introspection_agreement.graphql'],
  ['github', 'smear/tests/fixtures/schemas/github_schema.graphql'],
  ['gitlab', 'smear/tests/fixtures/schemas/gitlab_schema.graphql'],
  ['defaults', 'smear/tests/fixtures/schemas/introspection_defaults.graphql'],
];

/** Where the goldens go: beside `oracle.rs`'s, one directory per oracle. */
const OUT = 'smear/tests/oracle/graphql-js-introspection';

/**
 * `proto_introspection.rs`'s golden schema and query, whose smear answer is committed as
 * `smear/tests/golden/introspection_every_datum.json`. graphql-js's answer to the same query is
 * written beside the goldens so the recorded differences can be read off two committed files with
 * no toolchain at all. No test reads it.
 */
const EVERY_DATUM = 'smear/tests/proto_introspection.rs';

function sha256(bytes) {
  return createHash('sha256').update(bytes).digest('hex');
}

/** One Rust `r#"…"#` constant out of a test file, exactly as the test compiles it. */
function rustConstant(source, name) {
  const open = `const ${name}: &str = r#"`;
  const start = source.indexOf(open);
  if (start < 0 || source.indexOf(open, start + 1) >= 0) {
    throw new Error(`${EVERY_DATUM} does not declare exactly one \`${name}\``);
  }
  const from = start + open.length;
  const end = source.indexOf('"#;', from);
  if (end < 0) {
    throw new Error(`\`${name}\` in ${EVERY_DATUM} is not closed`);
  }
  return source.slice(from, end);
}

/** A golden: what was asked, of which SDL, and what graphql-js answered. */
function write(name, provenance, data) {
  const path = join(ROOT, OUT, `${name}.json`);
  writeFileSync(path, `${JSON.stringify({ provenance, data }, null, 1)}\n`);
  return path;
}

if (version !== RELEASE) {
  throw new Error(`graphql ${version} is installed and the goldens are ${RELEASE}'s; run \`npm ci\``);
}

const query = readFileSync(join(ROOT, QUERY), 'utf8');
if (getIntrospectionQuery(OPTIONS) !== query) {
  throw new Error(
    `${QUERY} is not getIntrospectionQuery(${JSON.stringify(OPTIONS)}) in graphql ${version}`,
  );
}

mkdirSync(join(ROOT, OUT), { recursive: true });

for (const [name, sdlPath] of FIXTURES) {
  const sdl = readFileSync(join(ROOT, sdlPath));
  const data = introspectionFromSchema(buildSchema(sdl.toString('utf8')), OPTIONS);
  const path = write(
    name,
    {
      graphql: version,
      generator: 'introspectionFromSchema(buildSchema(sdl), options)',
      options: OPTIONS,
      sdl: sdlPath,
      sdlSha256: sha256(sdl),
      query: QUERY,
      querySha256: sha256(query),
    },
    data,
  );
  console.log(`${name}: ${path}`);
}

{
  const source = readFileSync(join(ROOT, EVERY_DATUM), 'utf8');
  const sdl = rustConstant(source, 'GOLDEN_SDL');
  const golden = rustConstant(source, 'GOLDEN_QUERY');
  const result = graphqlSync({ schema: buildSchema(sdl), source: golden });
  if (result.errors) {
    throw new Error(`GOLDEN_QUERY raised: ${result.errors.map(String).join('; ')}`);
  }
  const path = write(
    'every_datum',
    {
      graphql: version,
      generator: 'graphqlSync({ schema: buildSchema(GOLDEN_SDL), source: GOLDEN_QUERY })',
      sdl: `${EVERY_DATUM}: GOLDEN_SDL`,
      sdlSha256: sha256(sdl),
      query: `${EVERY_DATUM}: GOLDEN_QUERY`,
      querySha256: sha256(golden),
      smear: 'smear/tests/golden/introspection_every_datum.json',
    },
    result.data,
  );
  console.log(`every_datum: ${path}`);
}
