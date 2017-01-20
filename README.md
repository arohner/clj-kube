Yet another Clojure bindings for Kubernetes

# Motivation

Q: Why not use the official Swagger?

  - The official swagger doesn't do a good job on Kubernetes:
    - Kubernete's swagger is
     - stuck on 1.2
     - Appears to be unidiomatic swagger:
     - the kube spec is spread across multiple .json files. Existing clj bindings don't expose e.g. `deployments` resource
     - kube API paths appear to unidiomatic with swagger's codegen
       - swagger turns `api/v1` into `apiv_.clj`

  - I don't like the swagger generated clojure names, especially `get-namespaced-foo`


# Usage

For each resource type, there are a series of functions in clj-kube.core, of the form

- `get-foo`
- `create-foo`
- `delete-foo`
- `ensure-foo`
- `foo-exists?`
- `list-foos`

Most commands take two required arguments: The url of the kube API
endpoint, and either the name a resource or the data to create
the resource.

All commands take an optional map as the last argument. Currently the
only option is `{:namespace "foo"}`. Namespaced resources (pods,
deployments, secrets, etc) all use `:namespace default`. Pass
:namespace to set the namespace, or `{:namespace nil}` to query all
resources.