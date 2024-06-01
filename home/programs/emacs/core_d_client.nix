{
  name,
  serverPath,
  configFile,
  writeCBin,
}:

writeCBin name ''
  #include <netinet/in.h>
  #include <stdbool.h>
  #include <stdio.h>
  #include <stdlib.h>
  #include <string.h>
  #include <sys/wait.h>
  #include <unistd.h>

  const char *server_path = "${serverPath}";
  const char *config_file = "${configFile}";

  static const char *get_runtime_dir() {
    const char *runtime_dir = getenv("XDG_RUNTIME_DIR");
    if (!runtime_dir) {
      runtime_dir = getenv("HOME");
      if (!runtime_dir) {
        fprintf(stderr, "get_runtime_dir: Missing HOME environment variable\n");
        return NULL;
      }
    }

    return runtime_dir;
  }

  typedef struct {
    unsigned short port;
    char token[16 + 1];
  } Config;

  static bool parse_config(const char *path, Config *config) {
    bool success = false;
    FILE *fp = fopen(path, "r");
    if (fp) {
      int num;
      if (fscanf(fp, "%hu %16s", &config->port, config->token) == 2) {
        success = fgetc(fp) == EOF;
      }

      fclose(fp);
    }

    return success;
  }

  static bool setup(const char *config_path, Config *config) {
    if (parse_config(config_path, config)) {
      return true;
    }

    pid_t child_pid = fork();
    if (child_pid == 0) {
      execl(server_path, server_path, "start");
      perror("setup: execl");
      return false;
    } else if (child_pid == -1) {
      perror("setup: fork");
      return false;
    }

    int status;
    if (waitpid(child_pid, &status, 0) == -1) {
      perror("setup: waitpid");
      return false;
    }

    if (WIFEXITED(status) && WEXITSTATUS(status) != 0 || WIFSIGNALED(status)) {
      fprintf(stderr, "setup: Failed to start server: %s start\n", server_path);
      if (WIFEXITED(status)) {
        fprintf(stderr, "exit code: %d\n", WEXITSTATUS(status));
      } else {
        fprintf(stderr, "signal: %d\n", WTERMSIG(status));
      }

      return false;
    }

    if (!parse_config(config_path, config)) {
      fprintf(stderr, "setup: Invalid config: %s\n", config_path);
      return false;
    }

    return true;
  }

  // TODO: Support cwd & args with spaces by encoding to JSON command format
  // TODO: Fix potential buffer overflow writing command to fixed-length buffer
  static bool write_command(
    int sock,
    Config *config,
    const char *cwd,
    int argc,
    const char *argv[]
  ) {
    char buffer[4096];
    unsigned short index = 0;
    memcpy(buffer + index, config->token, 16);
    index += 16;

    buffer[index++] = ' ';

    size_t cwd_len = strlen(cwd);
    memcpy(buffer + index, cwd, cwd_len);
    index += cwd_len;

    for (int i = 0; i < argc; i++) {
      buffer[index++] = ' ';

      const char *arg = argv[i];
      size_t arg_len = strlen(arg);
      memcpy(buffer + index, arg, arg_len);
      index += arg_len;
    }

    buffer[index++] = '\n';
    return write(sock, buffer, index) != -1;
  }

  static bool write_stdin(int sock) {
    char buffer[4096];
    ssize_t n_read;
    while ((n_read = read(STDIN_FILENO, buffer, sizeof(buffer))) > 0) {
      if (write(sock, buffer, n_read) == -1) {
        perror("write_stdin: write");
        return false;
      }
    }

    if (n_read == -1) {
      perror("write_stdin: read");
      return false;
    }

    return true;
  }

  typedef unsigned char ExitCode;
  typedef unsigned char ExitCodeState;

  static ExitCodeState scan_exit_code(
    ExitCodeState state,
    ExitCode *exit_code,
    char c
  ) {
    const char *prefix = "\n# exit ";
    switch (state) {
    default:
    start:
      if (c == prefix[0]) {
        return 1;
      } else {
        return 0;
      }
    case 1: case 2: case 3: case 4: case 5: case 6: case 7:
      if (c == prefix[state]) {
        return state + 1;
      } else {
        goto start;
      }
    case 8:
      if (c >= '0' && c <= '9') {
        *exit_code = c - '0';
        return state + 1;
      } else {
        goto start;
      }
    case 9:
      if (c >= '0' && c <= '9') {
        *exit_code = (*exit_code * 10) + (c - '0');
        return state;
      } else {
        goto start;
      }
    }
  }

  static bool has_exit_code(ExitCodeState state) {
    return state == 9;
  }

  static ExitCode process_stdout(int sock) {
    ExitCode exit_code;
    ExitCodeState exit_code_state = 0;
    char buffer[4096];
    ssize_t n_read;
    while ((n_read = read(sock, buffer, sizeof(buffer))) > 0) {
      if (write(STDOUT_FILENO, buffer, n_read) == -1) {
        perror("print_stdout: write");
        return 1;
      }

      for (size_t i = 0; i < n_read; ++i) {
        exit_code_state = scan_exit_code(exit_code_state, &exit_code, buffer[i]);
      }
    }

    if (n_read == -1) {
      perror("print_stdout: read");
      return 1;
    }

    if (has_exit_code(exit_code_state)) {
      return exit_code;
    } else {
      return 0;
    }
  }

  static ExitCode run_client(
    Config *config,
    const char *cwd,
    int argc,
    const char *argv[]
  ) {
    ExitCode exit_code = 1;
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    if (sock != -1) {
      struct sockaddr_in addr = {
        .sin_family = AF_INET,
        .sin_port = htons(config->port),
        .sin_addr = {
          .s_addr = htonl(INADDR_LOOPBACK),
        },
      };

      if (connect(sock, &addr, sizeof(addr)) != -1) {
        if (write_command(sock, config, cwd, argc, argv) && write_stdin(sock)) {
          if (shutdown(sock, SHUT_WR) != -1) {
            exit_code = process_stdout(sock);
          } else {
            perror("run_client: shutdown");
          }
        }
      } else {
        perror("run_client: connect");
      }
      close(sock);
    } else {
      perror("run_client: socket");
    }

    return exit_code;
  }

  int main(int argc, const char *argv[]) {
    if (argc <= 0) return 1;

    ExitCode exit_code = 1;
    char *cwd = getcwd(NULL, 0);
    if (cwd) {
      const char *runtime_dir = get_runtime_dir();
      if (runtime_dir) {
        char *config_path = malloc(strlen(runtime_dir) + 1 + strlen(config_file) + 1);
        if (config_path) {
          sprintf(config_path, "%s/%s", runtime_dir, config_file);
          Config config;
          if (setup(config_path, &config)) {
            free(config_path);
            exit_code = run_client(&config, cwd, argc - 1, &argv[1]);
          } else {
            free(config_path);
          }
        } else {
          perror("config_path: malloc");
        }
      }
      free(cwd);
    } else {
      perror("getcwd");
    }

    return exit_code;
  }
''
