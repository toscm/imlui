# IMLUI Installation Instruction

1. Install [R](https://cran.r-project.org/)

   Windows: enter the following commands in a `powershell.exe` console:

   ```powershell
   # 1. Install R
   $release_html_uri = "https://cran.r-project.org/bin/windows/base/release.html"
   $release_html = Invoke-WebRequest -Uri $release_html_uri
   $r_exe = $($release_html | Select-String "R-(.*?).exe").Matches.Value
   $r_exe_uri = "https://cran.r-project.org/bin/windows/base/$r_exe"
   $r_exe_loc = "~\Downloads\$r_exe"
   Invoke-WebRequest -Uri $r_exe_uri -OutFile $r_exe_loc
   & $r_exe_loc
   ```

   Ubuntu: Enter the following commands in a `bash` console:

   ```bash
   sudo apt install r-base
   ```

2. Install [git](https://git-scm.com/)
3. Configure [SSH access](https://docs.gitlab.com/ee/ssh/) to [gitlab.spang-lab.de](https://gitlab.spang-lab.de) for your account
4. Install the `devtools` and `imlui` package in `R`:
   ```R
    install.packages("devtools")
    url <- "ssh://git@gitlab.spang-lab.de:222/sct39258/imlui.git"
    Sys.setenv("R_REMOTES_STANDALONE"="true")
    remotes::install_git(url)
   ```
5. Windows only. Make `imlui` executable by entering following commands in `powershell`:
   ```powershell
   # Create $env:LOCALAPPDATA\Programs\imlui\imlui.bat
   $install_dir = "$env:LOCALAPPDATA\Programs\imlui"
   $command = '@R.exe --no-echo --vanilla -q -e "imlui:::cli()" --args %*'
   New-Item -ItemType Directory -Force -Path $install_dir
   Write-Output "$command" > "$install_dir\imlui.bat"
   # Add $install_dir to PATH env var
   $value_old = [Environment]::GetEnvironmentVariable("PATH", "User")
   if (-Not "$value_old".Contains("$install_dir")) {
       $value_new = $value_old + ";$install_dir"
       # Update user PATH in registry
       [Environment]::SetEnvironmentVariable("PATH", $value_new, "User")
       # Update PATH of current process
       $env:PATH = $env:PATH + ";$install_dir"
    }
   ```
6. Linux only. Make `imlui` executable by entering following commands in `bash`:
   ```bash
   # CODE BELOW NOT TESTED (TODO)
   echo '#!/usr/bin/env sh' > /usr/bin/imlui
   echo 'R --no-echo --vanilla -q -e "imlui:::cli()" --args "$@"' >> /usr/bin/imlui
   chmod u+x /usr/bin/imlui
   ```
