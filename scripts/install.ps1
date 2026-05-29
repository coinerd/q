# q installer for Windows — sets up Racket + q dependencies
# Usage: Invoke-Expression (Invoke-WebRequest -Uri https://raw.githubusercontent.com/coinerd/q/main/scripts/install.ps1).Content
# Or download and run: .\install.ps1

param(
    [string]$QDir = "$env:USERPROFILE\.q",
    [string]$RacketVersion = "8.10"
)

$ErrorActionPreference = "Stop"

Write-Host "=== q installer (Windows) ===" -ForegroundColor Cyan

# Check for Racket
$racket = Get-Command racket -ErrorAction SilentlyContinue
if ($racket) {
    $v = & racket --version 2>$null
    Write-Host "✓ Racket $v found"
} else {
    Write-Host "Installing Racket ${RacketVersion}..."
    $arch = if ([Environment]::Is64BitOperatingSystem) { "x86_64" } else { "i386" }
    $url = "https://download.racket-lang.org/installers/${RacketVersion}/racket-${RacketVersion}-${arch}-win32-cs.exe"
    $out = "$env:TEMP\racket-install.exe"
    Invoke-WebRequest -Uri $url -OutFile $out
    Start-Process -FilePath $out -ArgumentList "/S" -Wait
    Remove-Item $out -ErrorAction SilentlyContinue

    # Refresh PATH for this session
    $env:Path = [System.Environment]::GetEnvironmentVariable("Path", "Machine") + ";" + [System.Environment]::GetEnvironmentVariable("Path", "User")
    $racket = Get-Command racket -ErrorAction SilentlyContinue
    if (-not $racket) {
        Write-Error "Racket installation failed. Please install manually: https://racket-lang.org/install/"
        exit 1
    }
}

# Install q
Write-Host "Installing q..."
if (Test-Path $QDir) {
    Write-Host "Updating existing q installation at $QDir"
    Push-Location $QDir
    git pull
} else {
    git clone https://github.com/coinerd/q.git $QDir
    Push-Location $QDir
}

# Install dependencies
& raco pkg install --auto --batch 2>&1 | Write-Host

Pop-Location

# PATH guidance
Write-Host ""
Write-Host "=== q installed successfully ===" -ForegroundColor Green
Write-Host "Add to your PATH:"
Write-Host "  [Environment]::SetEnvironmentVariable('Path', \`$env:Path + ';$QDir', 'User')"
Write-Host ""
Write-Host "Then run: q --version"
Write-Host ""
Write-Host "NOTE: On Windows, store credentials via environment variables:"
Write-Host "  [Environment]::SetEnvironmentVariable('Q_OPENAI_API_KEY', 'sk-...', 'User')"
Write-Host "Or use the Windows Credential Manager (keychain support planned)."
