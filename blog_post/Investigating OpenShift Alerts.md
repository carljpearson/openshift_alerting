# Investigating OpenShift Alerts 

## How users percieve and act on them

Monitoring is a critical aspect of architecting and maintaining a successful OpenShift environment. One of the most common ways a user interacts with monitoring is through an alert, which can help direct the user's attention to the problem and, hopefully, how to resolve it. Our user experience design research team conducted a mixed-methods, online study with 89 internal OpenShift users and 14 OpenShift customers to explore (a) **what makes an alert useful** and (b) **what a user typically does after seeing an alert**. This information helped evaluate the current state of OpenShift alerts and guide what design changes in the web console could better support our users.

## Research Plan

 We found 11 especially frequent alerts by querying existing engineering data on active OpenShift clusters. These alerts served as the examples we used across our study. 

- **KubePodNotReady** Pod myproject/frontend-1 has been in a non-ready state for longer than 15 minutes.
- **KubeDeploymentReplicasMismatch Deployment** myproject/frontend-1 has not matched the expected number of replicas for longer than 15 minutes. 
- **KubeAPIErrorsHigh** API server is returning errors for 90% of requests.
- **MachineWithNoRunningPhase**  machine worker-us-east-1a is in unknown phase
- **TargetDown** 50% of the job distributor targets in myproject namespace are down.
- **KubeDaemonSetRolloutStuck** Only 25% of the desired Pods of DaemonSet myproject/frontend are scheduled and ready.
- **KubePodCrashLooping** Pod myproject/python-1 container python is restarting 36 times / 5 minutes. 
- **KubeletDown** Kubelet has disappeared from Prometheus target discovery.
- **CPUThrottlingHigh**  50% throttling of CPU in namespace myproject for container nodejs in pod nodejs-2.
- **etcdHighCommitDurations** etcd cluster "etcd": 99th percentile commit durations 100s on etcd instance cluster-health.
- **ClusterOperatorDegraded** Cluster operator insights has been degraded for 10 mins. Operator is degraded because Unable to report: gateway server reported unexpected error code: 415, and cluster upgrades will be unstable.

We used an online survey platform, *Qualtrics*, to implement our study. The survey began with background questions about the participants' experience levels and typical usage patterns. After, we displayed 5 of 11 alerts (randomly) to each user. After viewing an alert and it's accompanying alert description, we asked users how confident they felt in their ability to resolve the alert on a scale from 1-7, 1 being the least confidence and 7 being the most confident. We also allowed them to write in why they gave the numerical score they chose.

Unlike many applications and software, OpenShift is often deployed 'on-premise' so that standard product analytics are not possible to collect. This poses a unique challenge to assessing user behavior in OpenShift. To get around this, we implemented a [first-click test](https://www.usability.gov/how-to-and-tools/methods/first-click-testing.html) where users clicked on a web console overview dashboard screenshot. This allowed us to have some behavioral assessment on their strategy for resolving each alert. This first-click component occurred after each confidence rating with the same alert and allowed for an open response to describe why they clicked the area they did. 

![](https://github.com/carljpearson/openshift_alerting/blob/master/img/1-KubePodNotReady.png?raw=true)

## Results

### Participants 

Participants varied a lot in experience levels, but tended to be intermediate users with a high level of daily use. A fair proportion of users reported no experience, and these responses were excluded from some analyses. 

![](https://github.com/carljpearson/openshift_alerting/blob/master/plots/exp_and_use.png?raw=true)

The bulk of participants' version experience was with 3.x versions, but there were reports of experience with 4.x versions, but

