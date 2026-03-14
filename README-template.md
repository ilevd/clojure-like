# Clojure-like

List of Clojure-inspired programming languages, including ports, dialects, compilers, environments etc.

Updated on: **{{date}}**<br>
Total: **{{count}}**

{{table}}

* **Bold name** - last push was less than a year ago

## Usage

1. Add project to [repos.edn](repos.edn)
2. Optionaly add your Github token to `token.txt`
3. `lein run` for updating README.md

> [!WARNING]
> GitHub's rate limit is 60 requests per hour for unauthenticated requests. So, because number of projects more than 60, you should probably add your Github token to `token.txt`, or rerun generation after an hour. Results are cached, so generation will continue.
> <details><summary>How to create GitHub token</summary>
>
> * Settings -> Developer Settings -> Personal access tokens -> Fine-grained tokens -> Generate new token
> * Configure the token's details. 'Read-only access to public repositories.' is enough.
> * Generate token
></details>
