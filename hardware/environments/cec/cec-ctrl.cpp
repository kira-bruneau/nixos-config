#include <cstring>
#include <functional>
#include <iostream>

#include <chrono>
#include <thread>

#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/un.h>
#include <unistd.h>

#include "libcec/cec.h"
#include "libcec/cecloader.h"

using namespace CEC;

bool force_active(ICECAdapter *cec) {
  auto start = std::chrono::steady_clock::now();
  do {
    if (!cec->SetActiveSource(CEC_DEVICE_TYPE_PLAYBACK_DEVICE)) {
      return false;
    }

    std::this_thread::sleep_for(std::chrono::milliseconds(100));
  } while (!cec->IsLibCECActiveSource() && std::chrono::duration_cast<std::chrono::seconds>(std::chrono::steady_clock::now() - start).count() < 15);

  return true;
}

bool force_standby(ICECAdapter *cec) {
  return cec->StandbyDevices();
}

bool active(ICECAdapter *cec) {
  auto activeSource = cec->GetActiveSource();
  if (activeSource == CECDEVICE_UNKNOWN) {
    return force_active(cec);
  } else {
    switch (cec->GetDevicePowerStatus(activeSource)) {
      case CEC_POWER_STATUS_IN_TRANSITION_ON_TO_STANDBY:
      case CEC_POWER_STATUS_STANDBY:
        return force_active(cec);
      case CEC_POWER_STATUS_IN_TRANSITION_STANDBY_TO_ON:
      case CEC_POWER_STATUS_ON:
      case CEC_POWER_STATUS_UNKNOWN: {}
      }
  }

  return true;
}

bool inactive(ICECAdapter *cec) {
  return cec->SetInactiveView();
}

bool standby(ICECAdapter *cec) {
  auto activeSource = cec->GetActiveSource();
  if (activeSource == CECDEVICE_UNKNOWN || cec->GetLogicalAddresses().IsSet(activeSource)) {
    return force_standby(cec);
  }

  return true;
}

bool (*commands[])(ICECAdapter *) = {
  force_active,
  force_standby,
  active,
  inactive,
  standby,
};

enum class Command : uint8_t {
  force_active,
  force_standby,
  active,
  inactive,
  standby,
};

enum class ExitCode : uint8_t {
  success,
  invalid_command,
  socket,
  bind,
  fchmod,
  cec_initialize,
  cec_open,
  command,
};

ExitCode with_cec(const std::function<ExitCode (ICECAdapter *)> &f) {
  auto exit_code = ExitCode::success;

  libcec_configuration config;
  config.Clear();
  config.bActivateSource = 0;
  config.deviceTypes.Add(CEC_DEVICE_TYPE_PLAYBACK_DEVICE);

  auto *cec = LibCecInitialise(&config);
  if (cec) {
    cec_adapter_descriptor adapter;
    if (cec->DetectAdapters(&adapter, 1, NULL, true) && cec->Open(adapter.strComName)) {
      exit_code = f(cec);
      cec->Close();
    } else {
      exit_code = ExitCode::cec_open;
    }

    UnloadLibCec(cec);
  } else {
    exit_code = ExitCode::cec_initialize;
  }

  return exit_code;
}

ExitCode serve() {
  auto exit_code = ExitCode::success;

  auto sockfd = socket(AF_UNIX, SOCK_DGRAM, 0);
  if (sockfd != -1) {
    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, "/tmp/cec-ctrl", sizeof(addr.sun_path) - 1);
    unlink("/tmp/cec-ctrl");
    if (bind(sockfd, (struct sockaddr *)&addr, sizeof(addr)) == 0) {
      if (fchmod(sockfd, 0666) == 0) {
        exit_code = with_cec([sockfd = sockfd](auto cec) {
          if (!force_active(cec)) {
            return ExitCode::command;
          }

          uint8_t buffer[1024];
          ssize_t nbytes;
          while ((nbytes = recvfrom(sockfd, buffer, sizeof(buffer), 0, NULL, NULL)) != -1) {
            for (ssize_t i = 0; i < nbytes; ++i) {
              auto index = buffer[i];
              if (index < std::size(commands)) {
                if (!commands[index](cec)) {
                  return ExitCode::command;
                }
              }
            }
          }

          return ExitCode::success;
        });
      } else {
        exit_code = ExitCode::fchmod;
      }
    } else {
      exit_code = ExitCode::bind;
    }

    close(sockfd);
  } else {
    exit_code = ExitCode::socket;
  }

  return exit_code;
}

ExitCode send(Command command) {
  auto exit_code = ExitCode::success;

  auto sockfd = socket(AF_UNIX, SOCK_DGRAM, 0);
  if (sockfd != -1) {
    struct sockaddr_un addr;
    addr.sun_family = AF_UNIX;
    strncpy(addr.sun_path, "/tmp/cec-ctrl", sizeof(addr.sun_path) - 1);
    if (sendto(sockfd, &command, 1, 0, (struct sockaddr *)&addr, sizeof(addr)) != 1) {
      exit_code = with_cec([command = command](auto cec) {
        if (commands[static_cast<uint8_t>(command)](cec)) {
          return ExitCode::success;
        } else {
          return ExitCode::command;
        }
      });
    }

    close(sockfd);
  } else {
    exit_code = ExitCode::socket;
  }

  return exit_code;
}

int main(int argc, char *argv[]) {
  auto exit_code = ExitCode::invalid_command;

  if (argc > 1) {
    auto arg = argv[1];
    if (strcmp(arg, "serve") == 0) {
      exit_code = serve();
    } else if (strcmp(arg, "force_active") == 0) {
      exit_code = send(Command::force_active);
    } else if (strcmp(arg, "force_standby") == 0) {
      exit_code = send(Command::force_standby);
    } else if (strcmp(arg, "active") == 0) {
      exit_code = send(Command::active);
    } else if (strcmp(arg, "inactive") == 0) {
      exit_code = send(Command::inactive);
    } else if (strcmp(arg, "standby") == 0) {
      exit_code = send(Command::standby);
    }
  }

  return static_cast<int>(exit_code);
}
