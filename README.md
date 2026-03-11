# cl-tla-model-checker

Pure Common Lisp TLA+ temporal logic model checker with **zero external dependencies**.

## Features

- **State exploration**: Breadth-first and depth-first search
- **Temporal operators**: Always, Eventually, Until, Leads-to
- **Invariant checking**: Safety property verification
- **Liveness checking**: Progress property verification
- **Counterexamples**: Generate traces for violations
- **Pure Common Lisp**: No CFFI, no external libraries

## Installation

```lisp
(asdf:load-system :cl-tla-model-checker)
```

## Quick Start

```lisp
(use-package :cl-tla-model-checker)

;; Define a simple spec
(defspec mutex-spec
  :variables ((pc1 :init :idle)
              (pc2 :init :idle)
              (lock :init :free))
  :init (and (eq pc1 :idle) (eq pc2 :idle) (eq lock :free))
  :next (or (acquire 1) (acquire 2) (release 1) (release 2))
  :invariant (not (and (eq pc1 :critical) (eq pc2 :critical))))

;; Check the spec
(check-spec 'mutex-spec)
```

## API Reference

### Specification

- `(defspec name &key variables init next invariant)` - Define spec
- `(check-spec spec-name)` - Model check specification
- `(check-invariant spec invariant)` - Check safety property

### Temporal Logic

- `(always property)` - Property holds in all states
- `(eventually property)` - Property holds in some future state
- `(until p q)` - P holds until Q becomes true
- `(leads-to p q)` - P eventually leads to Q

### Results

- `(get-counterexample result)` - Get violation trace
- `(get-state-count result)` - Get explored state count

## Testing

```lisp
(asdf:test-system :cl-tla-model-checker)
```

## License

BSD-3-Clause

Copyright (c) 2024-2026 Parkian Company LLC. All rights reserved.
