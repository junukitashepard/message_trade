# MESSAGE-TRADE

MESSAGE-TRADE is an extension on the MESSAGEix global energy model. MESSAGE-TRADE allows users to model international trade in fuels bilaterally, instead of as a global pool.

## Background
Text is sourced from Shepard, Jun. "Representing Shipping and Transport Networks in Global Energy Models", International Institute for Applied Systems Analysis (2019).

[MESSAGEix](https://github.com/iiasa/message_ix) currently represents the world through [14 regions](http://www.iiasa.ac.at/web/home/research/researchPrograms/Energy/MESSAGE-model-regions.en.html). Trade is represented through a global pool framework. In this framework, regions with excess energy resources can export to a global pool from which regions that demand that resource can import. Figure 1(a) below illustrates the global pool schema as a map, while Figure 1(b) represents the schema as a flow of energy. 

![Alt text](images/global_pool_map.png "Figure 1(a) Hypothetical map of Global Pool Schema")

Here, commodities represent the energy resource (e.g. LNG) while the technology represents the movement of the commodity (e.g. LNG exports, LNG imports). The technology is differentiated by commodity but not by location. This is what defines the global pool schema.

Note that while the global pool schema allows us to examine the total exports from/imports to regions, it does not explicitly model the trade flows among them. This explicit bilateral representation is necessary to model trade as a network and to measure security indices like trade portfolio diversity and import dependence.

The bilateral trade representation is illustrated in Figure 2(a) and 2(b). In order to explicitly delineate bilateral trade flows among regions, we needed to completely reparametrize trade in MESSAGEIX. Note that in the global pool schema, the origin of fuel imports and the destination of fuel exports are not explicit in the commodity or technology. In the bilateral framework we explicitly define the destination of commodity imports (e.g. LNG_weu means LNG imports to Western Europe) and export technologies (e.g. LNG_exp_weu means LNG exports from the given region that are destined for Western Europe). This is illustrated in Figure 2(b). 



```bash
pip install foobar
```

## Usage

```python
import foobar

foobar.pluralize('word') # returns 'words'
foobar.pluralize('goose') # returns 'geese'
foobar.singularize('phenomena') # returns 'phenomenon'
```

## Contributing
Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

Please make sure to update tests as appropriate.

## License
[MIT](https://choosealicense.com/licenses/mit/)