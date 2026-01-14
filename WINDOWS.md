# Running on Windows

## Quick Start (Without GnuCOBOL)

If you just want to see the demo without installing GnuCOBOL:

```cmd
demo.bat
```

This runs an interactive simulation showing the system's functionality.

## Building on Windows (Requires GnuCOBOL)

### Option 1: Install GnuCOBOL for Windows

1. Download from: https://sourceforge.net/projects/gnucobol/
2. Install following the installer instructions
3. Add to PATH if not done automatically
4. Open Command Prompt or PowerShell
5. Navigate to project directory
6. Run:

```cmd
build.bat
run.bat
```

### Option 2: Use WSL (Windows Subsystem for Linux)

1. Install WSL2:
```powershell
wsl --install
```

2. Inside WSL, install GnuCOBOL:
```bash
sudo apt-get update
sudo apt-get install gnucobol
```

3. Navigate to project and run:
```bash
./build.sh
./run.sh
```

### Option 3: Use Docker

Create a `Dockerfile`:
```dockerfile
FROM ubuntu:24.04
RUN apt-get update && apt-get install -y gnucobol
WORKDIR /app
COPY . .
RUN ./build.sh
CMD ["./run.sh"]
```

Then:
```cmd
docker build -t banking-system .
docker run -it banking-system
```

## Files Explained

- `build.bat` - Windows build script (requires GnuCOBOL)
- `run.bat` - Windows run script
- `demo.bat` - Interactive demo (no GnuCOBOL needed)
- `build.sh` - Linux/Mac build script
- `run.sh` - Linux/Mac run script
- `demo.sh` - Linux/Mac demo script

## Troubleshooting

### "cobc not found"
- Install GnuCOBOL or use WSL
- Or just run `demo.bat` to see the simulation

### Scripts don't execute
- Use Command Prompt (not PowerShell for .bat files)
- Or run: `cmd /c build.bat`

### Permission denied
- Run as Administrator if needed

## Note for Windows Users

This project was designed for Linux/Unix environments (where COBOL is traditionally used). The Windows scripts are provided for convenience, but the best experience is through:

1. WSL (recommended)
2. Docker
3. Native GnuCOBOL for Windows

The demo (`demo.bat`) works on any Windows system without dependencies.
