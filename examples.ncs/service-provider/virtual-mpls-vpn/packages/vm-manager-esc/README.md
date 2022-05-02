# Introduction

This package is used in combination with the vm-manager package.
It provides a bridge between the vm-manager and the ESC, and
is responsible for starting and stopping VMs requested through
the vm-manager API.

It assumes that all ESC devices are mounted in the /devices/device
tree, and that ESC service registrations have been performed.

This package represents a reasonable first stab at vm-manager ESC
integration and should be considered a starting point.

