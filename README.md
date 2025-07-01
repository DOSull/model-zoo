# model-zoo
Model zoo (in Netlogo) for the book _Spatial Simulation: Exploring Pattern and Process_.

This is the collection of [Netlogo](http://github.com/NetLogo) models we built for our book [_Spatial Simulation: Exploring
Pattern and Process_](http://www.wiley.com/WileyCDA/WileyTitle/productCd-1119970792.html). The models are also available at
[the book website](http://dosull.github.io/pattern-and-process).

The purpose of putting the models here, is to speed up the process of adequately commenting them, and also to make it
easier to make minor tweaks and corrections to the code.  

The models as originally released when the book appeared (and as used to generate the figures in the book) are in the folder [`as-released`](/as-released). **These will not change**, and **will not be updated** to remain compatible with later versions of NetLogo. They were written for NetLogo 5.0.x. They will likely work with other NetLogo 5.x versions, but you will need to have the necessary extensions installed. If opened in later versions of NetLogo, the automated translation provided may work, but it might not. This folder is split into a `base-models` folder which should not need any extensions and a `using-R` folder which requires the `r` extension.

## Ongoing work
Models are being slowly updated, to add more information about how they work and additional in-code comments, and more recently for compatibility with NetLogo 6.x.  The latest versions of the models are in the top level [`base-models`](/base-models) folder. Some models require the NetLogo R extension to be installed or the more recent [SimpleR extension](https://github.com/NetLogo/SimpleR-Extension).

A 'frozen' release of these folders compatible with NetLogo 5.3 was created on 18 November, 2017: [v0.5.3 release](https://github.com/DOSull/model-zoo/releases/tag/v0.5.3)

A 'final for now' release of these folders compatible with NetLogo 6.4.x was created on 2 August, 2024: [v0.6.4 release](https://github.com/DOSull/model-zoo/releases/tag/v0.6.4).

The most current code in `base-models` and `using-R` folders aims to be compatible with current recent versions of NetLogo.

## Current status
Models in `base-models` and `using-R` should all work with NetLogo 6.4.x. Models in `as-released` and `misc` will not. The models now use only extensions that come standard with NetLogo (r and palette).
