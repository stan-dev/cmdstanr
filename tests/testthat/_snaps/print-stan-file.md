# print_stan_file() prints plain code outside of knitr

    Code
      cat(out, sep = "\n")
    Output
      
      parameters {
        real y;
      }
      model {
        y ~ std_normal();
      }
      

# print_stan_file() outputs fenced code block in knitr with results='asis'

    Code
      cat(out, sep = "\n")
    Output
      ```stan
      
      parameters {
        real y;
      }
      model {
        y ~ std_normal();
      }
      
      
      ```

# print_stan_file() wraps in <details> when fold=TRUE

    Code
      cat(out, sep = "\n")
    Output
      <details><summary>Stan model code</summary>
      
      ```stan
      
      parameters {
        real y;
      }
      model {
        y ~ std_normal();
      }
      
      
      ```
      
      </details>

# print_stan_file() uses custom summary text

    Code
      cat(out, sep = "\n")
    Output
      <details><summary>My Stan Code</summary>
      
      ```stan
      
      parameters {
        real y;
      }
      model {
        y ~ std_normal();
      }
      
      
      ```
      
      </details>

# print_stan_file() does not fold when fold=FALSE in knitr

    Code
      cat(out, sep = "\n")
    Output
      ```stan
      
      parameters {
        real y;
      }
      model {
        y ~ std_normal();
      }
      
      
      ```

# print_stan_file() falls back to plain text without results='asis'

    Code
      cat(out, sep = "\n")
    Output
      
      parameters {
        real y;
      }
      model {
        y ~ std_normal();
      }
      

# print_stan_file() falls back to plain text without knitr.in.progress

    Code
      cat(out, sep = "\n")
    Output
      
      parameters {
        real y;
      }
      model {
        y ~ std_normal();
      }
      

