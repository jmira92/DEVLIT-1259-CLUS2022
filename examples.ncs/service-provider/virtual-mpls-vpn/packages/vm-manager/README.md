# Introduction

The problem we are trying to solve is that of a generic vm start
mechanism that works well with services and in a high availability
(HA) configuration.

This package provides the API for services that want to start VMs. It
is intended to work together with another package that talks to a VIM
(Virtual Infrastructure Manager) or VNF-M (VNF-Manager), for example
the ESC.

We want to be able to hook up different VIM/VNF-Ms in the backend,
just as the resource-manager makes it possible to interface towards
different IPAM without the service necessarily being aware of which
IPAM is used.

# Usage

A service that want to start a vm creates an entry in the
/vmm:vm-manager/start list and provides the information needed by the
VIM/VNF-M, and calls the VmManager.registerStartRequest().  This will
add the service to the /vm-manager/start{}/allocators list.

The vm-manager package must be used together with an implementation
package that interfaces with the specific VIM/VNF-M, lets call that
package vm-manager-x.

The contract with the vm-manager-x package is that it should
do the following when a new /vm-manager/start{} entry is created

1. start a VM as specified by the parameters in vm-manager/start

2. handle all event processing towards the VIM/VNF-M

3. mount the newly started VM in the devices tree, and perform
   fetch-ssh-keys and sync-from

4. add the device to the /vm-manager/start{}/device list

5. (optional) load a license for the VM. This may also be
   part of the day1 config that the service applies to the
   device.

6. set the /devices/device/vmm:ready leaf to true

7. re-deploy the service

And when a list entry is deleted it should do the following:

1. (optional) Release the license.

2. Remove the devices mounted for the request from the /devices/device
   list. Note that due to scale-in/scale-out multiple devices may have
   been created from a single vm-manager/start entry.

3. Instruct the VIM/VNF-M to stop the VM.

## Failure handling

In case the implementing package fails to start the VM or
the VM dies and cannot be re-initialized, the vm-manager
should set the /vm-manager/start{}/error-status leaf with a proper
response and re-deploy all services registered under
/vm-manager/start{}/allocators.

If the VM is re-initialized, ie restarted without its configuration
the registered services should be re-deployed.

A failure to delete a VM should be signaled with an
alarm (this is not implemented in the vm-manager-esc
package at the moment).

# Extending the API

A specific VIM/VNF-M may need a different set of inputs to start a VM.
It may then augment the /vm-manager/start{} tree with the additional
parameters it needs, and similarly augment in additional
status/response structures as config false nodes.

The current contents of the vm-manager/start list is heavily influenced by
what the ESC needs in terms of input parameters. It is likely that
these will have to change when other VIMs/VNF-Ms are used. However,
the overall architecture should remain the same.

# Staged Create and Delete

The problems with starting a VM directly from a service by creating
the right configuration in the VIM/VNF-M device are several:

1. As seen above starting a VM is a multi stage process where
   events needs to be handled and reacted upon. The device needs
   to be added to the device tree, synced etc.

2. When the service is deleted NCS will remove all configuration that
   was created by the service. If the service both started the device
   directly, and configured the device itself, there will be a
   problem. The VIM/VNF-M will be instructed to close down the VM at
   the same time as NCS is trying to undo the configuration on the VM
   that the service created. This results in a race condition that
   often ends up with an error. Also, in many cases a license needs
   to be released as part of shutting down the VM. This cannot
   be handled gracefully while the VM is being torn down by the
   VIM/VNF-M.

Using this vm-manager package provides a solution. Since the service
requests the VM to be started by creating a vm-manager/start entry,
when deleted only the vm-manager/start entry will be removed in that
transaction.  This will not immediately result in the VM being torn
down since the vm-manager-x package first has to be notified that the
vm-manager/start entry is removed before it can modify the
configuration of the VIM/VNF-M in a new transaction. This means that
the first transaction can remove the configuration from the VM without
interruption. Similarly, the vm-manager-x package may take other
actions, like releasing a license, before shutting down the VM.

Note that it is important that the inner service, ie the service in
the vm-manager-x, only modifies the configuration of the VIM/VNF-M and
not the configuration of the started VM. If it also modifies the
started VM (for example applies a day1 configuration) there will be a
problem when deleting the service since NCS will then both tell the
VIM/VNF-M to stop the VM and at the same time undo the day1
configuration.

A much better solution is to let the service that created the
vm-manager/start request apply the day1 configuration. That way the
teardown will work as expected, and if the VM is re-initialized, ie
restarted from scratch and loosing its configuration) a re-deploy of
the outer service will restore the day1 configuration to the device.

