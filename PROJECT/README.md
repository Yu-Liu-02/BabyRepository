In the study conducted by Steinmetz et al. (2019), experiments were performed on a total of 10 mice over 39 sessions. Each session comprised several hundred trials, during which visual stimuli were randomly presented to the mouse on two screens positioned on both sides of it. The stimuli varied in terms of contrast levels, which took values in {0, 0.25, 0.5, 1}, with 0 indicating the absence of a stimulus. The mice were required to make decisions based on the visual stimuli, using a wheel controlled by their forepaws. A reward or penalty (i.e., feedback) was subsequently administered based on the outcome of their decisions. In particular,

When left contrast > right contrast, success (1) if turning the wheel to the right and failure (-1) otherwise.<br>
When right contrast > left contrast, success (1) if turning the wheel to the left and failure (-1) otherwise.<br>
When both left and right contrasts are zero, success (1) if holding the wheel still and failure (-1) otherwise.<br>
When left and right contrasts are equal but non-zero, left or right will be randomly chosen (50%) as the correct choice.<br>

The activity of the neurons in the mice’s visual cortex was recorded during the trials and made available in the form of spike trains. which are collections of timestamps corresponding to neuron firing. In this project, we focus specifically on the spike trains of neurons from the onset of the stimuli to 0.4 seconds post-onset. In addition, we only use Sessions 1 to 3 from one mouse named Cori.

One RDS file accords to the records from 1 session. In each RDS file, one can find the name of mouse from `mouse_name` and date of the experiment from `date_exp`. Moreover, five variables are available in each trial, namely <br>
$\cdot$ `feedback_type`: 1 for success and -1 for failure <br> 
$\cdot$ `contrast_left`: contrast of the left stimulus <br>
$\cdot$ `contrast_right`: contrast of the right stimulus <br>
$\cdot$ `time`: centers of the time bins for `spks` <br>
$\cdot$ `spks`: numbers of spikes of neurons in the visual cortex in time bins defined in `time` <br>
$\cdot$ `brain_area`: area of the brain where each neuron lives <br>

Something may be interesting: <br>
$\cdot$ Are the behaviors of different sessions among the same mice homogenous or heterogeneous? <br>
$\cdot$ Are there any shared patterns among different sessions? <br>
$\cdot$ Are there some brain areas correlated closely during the experiments? <br>
$\cdot$ As the time goes, does the mouse Cori behave better (can identify which is the stronger contrast)? <br>
$\cdot$ Can we predict the choice that the mouse Cori will choose based on the spike trains? <br>
