from tkinter import *
import numpy as np
from tkinter.font import Font
import math

wordle_list = ['ABACK','ABASE','ABATE','ABBEY','ABBOT','ABHOR','ABIDE','ABLED','ABODE','ABORT','ABOUT','ABOVE','ABUSE','ABYSS','ACORN','ACRID','ACTOR','ACUTE','ADAGE','ADAPT','ADEPT','ADMIN','ADMIT','ADOBE','ADOPT','ADORE','ADORN','ADULT','AFFIX','AFIRE','AFOOT','AFOUL','AFTER','AGAIN','AGAPE','AGATE','AGENT','AGILE','AGING','AGLOW','AGONY','AGORA','AGREE','AHEAD','AIDER','AISLE','ALARM','ALBUM','ALERT','ALGAE','ALIBI','ALIEN','ALIGN','ALIKE','ALIVE','ALLAY','ALLEY','ALLOT','ALLOW','ALLOY','ALOFT','ALONE','ALONG','ALOOF','ALOUD','ALPHA','ALTAR','ALTER','AMASS','AMAZE','AMBER','AMBLE','AMEND','AMISS','AMITY','AMONG','AMPLE','AMPLY','AMUSE','ANGEL','ANGER','ANGLE','ANGRY','ANGST','ANIME','ANKLE','ANNEX','ANNOY','ANNUL','ANODE','ANTIC','ANVIL','AORTA','APART','APHID','APING','APNEA','APPLE','APPLY','APRON','APTLY','ARBOR','ARDOR','ARENA','ARGUE','ARISE','ARMOR','AROMA','AROSE','ARRAY','ARROW','ARSON','ARTSY','ASCOT','ASHEN','ASIDE','ASKEW','ASSAY','ASSET','ATOLL','ATONE','ATTIC','AUDIO','AUDIT','AUGUR','AUNTY','AVAIL','AVERT','AVIAN','AVOID','AWAIT','AWAKE','AWARD','AWARE','AWASH','AWFUL','AWOKE','AXIAL','AXIOM','AXION','AZURE','BACON','BADGE','BADLY','BAGEL','BAGGY','BAKER','BALER','BALMY','BANAL','BANJO','BARGE','BARON','BASAL','BASIC','BASIL','BASIN','BASIS','BASTE','BATCH','BATHE','BATON','BATTY','BAWDY','BAYOU','BEACH','BEADY','BEARD','BEAST','BEECH','BEEFY','BEFIT','BEGAN','BEGAT','BEGET','BEGIN','BEGUN','BEING','BELCH','BELIE','BELLE','BELLY','BELOW','BENCH','BERET','BERRY','BERTH','BESET','BETEL','BEVEL','BEZEL','BIBLE','BICEP','BIDDY','BIGOT','BILGE','BILLY','BINGE','BINGO','BIOME','BIRCH','BIRTH','BISON','BITTY','BLACK','BLADE','BLAME','BLAND','BLANK','BLARE','BLAST','BLAZE','BLEAK','BLEAT','BLEED','BLEEP','BLEND','BLESS','BLIMP','BLIND','BLINK','BLISS','BLITZ','BLOAT','BLOCK','BLOKE','BLOND','BLOOD','BLOOM','BLOWN','BLUER','BLUFF','BLUNT','BLURB','BLURT','BLUSH','BOARD','BOAST','BOBBY','BONEY','BONGO','BONUS','BOOBY','BOOST','BOOTH','BOOTY','BOOZE','BOOZY','BORAX','BORNE','BOSOM','BOSSY','BOTCH','BOUGH','BOULE','BOUND','BOWEL','BOXER','BRACE','BRAID','BRAIN','BRAKE','BRAND','BRASH','BRASS','BRAVE','BRAVO','BRAWL','BRAWN','BREAD','BREAK','BREED','BRIAR','BRIBE','BRICK','BRIDE','BRIEF','BRINE','BRING','BRINK','BRINY','BRISK','BROAD','BROIL','BROKE','BROOD','BROOK','BROOM','BROTH','BROWN','BRUNT','BRUSH','BRUTE','BUDDY','BUDGE','BUGGY','BUGLE','BUILD','BUILT','BULGE','BULKY','BULLY','BUNCH','BUNNY','BURLY','BURNT','BURST','BUSED','BUSHY','BUTCH','BUTTE','BUXOM','BUYER','BYLAW','CABAL','CABBY','CABIN','CABLE','CACAO','CACHE','CACTI','CADDY','CADET','CAGEY','CAIRN','CAMEL','CAMEO','CANAL','CANDY','CANNY','CANOE','CANON','CAPER','CAPUT','CARAT','CARGO','CAROL','CARRY','CARVE','CASTE','CATCH','CATER','CATTY','CAULK','CAUSE','CAVIL','CEASE','CEDAR','CELLO','CHAFE','CHAFF','CHAIN','CHAIR','CHALK','CHAMP','CHANT','CHAOS','CHARD','CHARM','CHART','CHASE','CHASM','CHEAP','CHEAT','CHECK','CHEEK','CHEER','CHESS','CHEST','CHICK','CHIDE','CHIEF','CHILD','CHILI','CHILL','CHIME','CHINA','CHIRP','CHOCK','CHOIR','CHOKE','CHORD','CHORE','CHOSE','CHUCK','CHUMP','CHUNK','CHURN','CHUTE','CIDER','CIGAR','CINCH','CIRCA','CIVIC','CIVIL','CLACK','CLAIM','CLAMP','CLANG','CLANK','CLASH','CLASP','CLASS','CLEAN','CLEAR','CLEAT','CLEFT','CLERK','CLICK','CLIFF','CLIMB','CLING','CLINK','CLOAK','CLOCK','CLONE','CLOSE','CLOTH','CLOUD','CLOUT','CLOVE','CLOWN','CLUCK','CLUED','CLUMP','CLUNG','COACH','COAST','COBRA','COCOA','COLON','COLOR','COMET','COMFY','COMIC','COMMA','CONCH','CONDO','CONIC','COPSE','CORAL','CORER','CORNY','COUCH','COUGH','COULD','COUNT','COUPE','COURT','COVEN','COVER','COVET','COVEY','COWER','COYLY','CRACK','CRAFT','CRAMP','CRANE','CRANK','CRASH','CRASS','CRATE','CRAVE','CRAWL','CRAZE','CRAZY','CREAK','CREAM','CREDO','CREED','CREEK','CREEP','CREME','CREPE','CREPT','CRESS','CREST','CRICK','CRIED','CRIER','CRIME','CRIMP','CRISP','CROAK','CROCK','CRONE','CRONY','CROOK','CROSS','CROUP','CROWD','CROWN','CRUDE','CRUEL','CRUMB','CRUMP','CRUSH','CRUST','CRYPT','CUBIC','CUMIN','CURIO','CURLY','CURRY','CURSE','CURVE','CURVY','CUTIE','CYBER','CYCLE','CYNIC','DADDY','DAILY','DAIRY','DAISY','DALLY','DANCE','DANDY','DATUM','DAUNT','DEALT','DEATH','DEBAR','DEBIT','DEBUG','DEBUT','DECAL','DECAY','DECOR','DECOY','DECRY','DEFER','DEIGN','DEITY','DELAY','DELTA','DELVE','DEMON','DEMUR','DENIM','DENSE','DEPOT','DEPTH','DERBY','DETER','DETOX','DEUCE','DEVIL','DIARY','DICEY','DIGIT','DILLY','DIMLY','DINER','DINGO','DINGY','DIODE','DIRGE','DIRTY','DISCO','DITCH','DITTO','DITTY','DIVER','DIZZY','DODGE','DODGY','DOGMA','DOING','DOLLY','DONOR','DONUT','DOPEY','DOUBT','DOUGH','DOWDY','DOWEL','DOWNY','DOWRY','DOZEN','DRAFT','DRAIN','DRAKE','DRAMA','DRANK','DRAPE','DRAWL','DRAWN','DREAD','DREAM','DRESS','DRIED','DRIER','DRIFT','DRILL','DRINK','DRIVE','DROIT','DROLL','DRONE','DROOL','DROOP','DROSS','DROVE','DROWN','DRUID','DRUNK','DRYER','DRYLY','DUCHY','DULLY','DUMMY','DUMPY','DUNCE','DUSKY','DUSTY','DUTCH','DUVET','DWARF','DWELL','DWELT','DYING','EAGER','EAGLE','EARLY','EARTH','EASEL','EATEN','EATER','EBONY','ECLAT','EDICT','EDIFY','EERIE','EGRET','EIGHT','EJECT','EKING','ELATE','ELBOW','ELDER','ELECT','ELEGY','ELFIN','ELIDE','ELITE','ELOPE','ELUDE','EMAIL','EMBED','EMBER','EMCEE','EMPTY','ENACT','ENDOW','ENEMA','ENEMY','ENJOY','ENNUI','ENSUE','ENTER','ENTRY','ENVOY','EPOCH','EPOXY','EQUAL','EQUIP','ERASE','ERECT','ERODE','ERROR','ERUPT','ESSAY','ESTER','ETHER','ETHIC','ETHOS','ETUDE','EVADE','EVENT','EVERY','EVICT','EVOKE','EXACT','EXALT','EXCEL','EXERT','EXILE','EXIST','EXPEL','EXTOL','EXTRA','EXULT','EYING','FABLE','FACET','FAINT','FAIRY','FAITH','FALSE','FANCY','FANNY','FARCE','FATAL','FATTY','FAULT','FAUNA','FAVOR','FEAST','FECAL','FEIGN','FELLA','FELON','FEMME','FEMUR','FENCE','FERAL','FERRY','FETAL','FETCH','FETID','FETUS','FEVER','FEWER','FIBER','FIBRE','FICUS','FIELD','FIEND','FIERY','FIFTH','FIFTY','FIGHT','FILER','FILET','FILLY','FILMY','FILTH','FINAL','FINCH','FINER','FIRST','FISHY','FIXER','FIZZY','FJORD','FLACK','FLAIL','FLAIR','FLAKE','FLAKY','FLAME','FLANK','FLARE','FLASH','FLASK','FLECK','FLEET','FLESH','FLICK','FLIER','FLING','FLINT','FLIRT','FLOAT','FLOCK','FLOOD','FLOOR','FLORA','FLOSS','FLOUR','FLOUT','FLOWN','FLUFF','FLUID','FLUKE','FLUME','FLUNG','FLUNK','FLUSH','FLUTE','FLYER','FOAMY','FOCAL','FOCUS','FOGGY','FOIST','FOLIO','FOLLY','FORAY','FORCE','FORGE','FORGO','FORTE','FORTH','FORTY','FORUM','FOUND','FOYER','FRAIL','FRAME','FRANK','FRAUD','FREAK','FREED','FREER','FRESH','FRIAR','FRIED','FRILL','FRISK','FRITZ','FROCK','FROND','FRONT','FROST','FROTH','FROWN','FROZE','FRUIT','FUDGE','FUGUE','FULLY','FUNGI','FUNKY','FUNNY','FUROR','FURRY','FUSSY','FUZZY','GAFFE','GAILY','GAMER','GAMMA','GAMUT','GASSY','GAUDY','GAUGE','GAUNT','GAUZE','GAVEL','GAWKY','GAYER','GAYLY','GAZER','GECKO','GEEKY','GEESE','GENIE','GENRE','GHOST','GHOUL','GIANT','GIDDY','GIPSY','GIRLY','GIRTH','GIVEN','GIVER','GLADE','GLAND','GLARE','GLASS','GLAZE','GLEAM','GLEAN','GLIDE','GLINT','GLOAT','GLOBE','GLOOM','GLORY','GLOSS','GLOVE','GLYPH','GNASH','GNOME','GODLY','GOING','GOLEM','GOLLY','GONAD','GONER','GOODY','GOOEY','GOOFY','GOOSE','GORGE','GOUGE','GOURD','GRACE','GRADE','GRAFT','GRAIL','GRAIN','GRAND','GRANT','GRAPE','GRAPH','GRASP','GRASS','GRATE','GRAVE','GRAVY','GRAZE','GREAT','GREED','GREEN','GREET','GRIEF','GRILL','GRIME','GRIMY','GRIND','GRIPE','GROAN','GROIN','GROOM','GROPE','GROSS','GROUP','GROUT','GROVE','GROWL','GROWN','GRUEL','GRUFF','GRUNT','GUARD','GUAVA','GUESS','GUEST','GUIDE','GUILD','GUILE','GUILT','GUISE','GULCH','GULLY','GUMBO','GUMMY','GUPPY','GUSTO','GUSTY','GYPSY','HABIT','HAIRY','HALVE','HANDY','HAPPY','HARDY','HAREM','HARPY','HARRY','HARSH','HASTE','HASTY','HATCH','HATER','HAUNT','HAUTE','HAVEN','HAVOC','HAZEL','HEADY','HEARD','HEART','HEATH','HEAVE','HEAVY','HEDGE','HEFTY','HEIST','HELIX','HELLO','HENCE','HERON','HILLY','HINGE','HIPPO','HIPPY','HITCH','HOARD','HOBBY','HOIST','HOLLY','HOMER','HONEY','HONOR','HORDE','HORNY','HORSE','HOTEL','HOTLY','HOUND','HOUSE','HOVEL','HOVER','HOWDY','HUMAN','HUMID','HUMOR','HUMPH','HUMUS','HUNCH','HUNKY','HURRY','HUSKY','HUSSY','HUTCH','HYDRO','HYENA','HYMEN','HYPER','ICILY','ICING','IDEAL','IDIOM','IDIOT','IDLER','IDYLL','IGLOO','ILIAC','IMAGE','IMBUE','IMPEL','IMPLY','INANE','INBOX','INCUR','INDEX','INEPT','INERT','INFER','INGOT','INLAY','INLET','INNER','INPUT','INTER','INTRO','IONIC','IRATE','IRONY','ISLET','ISSUE','ITCHY','IVORY','JAUNT','JAZZY','JELLY','JERKY','JETTY','JEWEL','JIFFY','JOINT','JOIST','JOKER','JOLLY','JOUST','JUDGE','JUICE','JUICY','JUMBO','JUMPY','JUNTA','JUNTO','JUROR','KAPPA','KARMA','KAYAK','KEBAB','KHAKI','KINKY','KIOSK','KITTY','KNACK','KNAVE','KNEAD','KNEED','KNEEL','KNELT','KNIFE','KNOCK','KNOLL','KNOWN','KOALA','KRILL','LABEL','LABOR','LADEN','LADLE','LAGER','LANCE','LANKY','LAPEL','LAPSE','LARGE','LARVA','LASSO','LATCH','LATER','LATHE','LATTE','LAUGH','LAYER','LEACH','LEAFY','LEAKY','LEANT','LEAPT','LEARN','LEASE','LEASH','LEAST','LEAVE','LEDGE','LEECH','LEERY','LEFTY','LEGAL','LEGGY','LEMON','LEMUR','LEPER','LEVEL','LEVER','LIBEL','LIEGE','LIGHT','LIKEN','LILAC','LIMBO','LIMIT','LINEN','LINER','LINGO','LIPID','LITHE','LIVER','LIVID','LLAMA','LOAMY','LOATH','LOBBY','LOCAL','LOCUS','LODGE','LOFTY','LOGIC','LOGIN','LOOPY','LOOSE','LORRY','LOSER','LOUSE','LOUSY','LOVER','LOWER','LOWLY','LOYAL','LUCID','LUCKY','LUMEN','LUMPY','LUNAR','LUNCH','LUNGE','LUPUS','LURCH','LURID','LUSTY','LYING','LYMPH','LYNCH','LYRIC','MACAW','MACHO','MACRO','MADAM','MADLY','MAFIA','MAGIC','MAGMA','MAIZE','MAJOR','MAKER','MAMBO','MAMMA','MAMMY','MANGA','MANGE','MANGO','MANGY','MANIA','MANIC','MANLY','MANOR','MAPLE','MARCH','MARRY','MARSH','MASON','MASSE','MATCH','MATEY','MAUVE','MAXIM','MAYBE','MAYOR','MEALY','MEANT','MEATY','MECCA','MEDAL','MEDIA','MEDIC','MELEE','MELON','MERCY','MERGE','MERIT','MERRY','METAL','METER','METRO','MICRO','MIDGE','MIDST','MIGHT','MILKY','MIMIC','MINCE','MINER','MINIM','MINOR','MINTY','MINUS','MIRTH','MISER','MISSY','MOCHA','MODAL','MODEL','MODEM','MOGUL','MOIST','MOLAR','MOLDY','MONEY','MONTH','MOODY','MOOSE','MORAL','MORON','MORPH','MOSSY','MOTEL','MOTIF','MOTOR','MOTTO','MOULT','MOUND','MOUNT','MOURN','MOUSE','MOUTH','MOVER','MOVIE','MOWER','MUCKY','MUCUS','MUDDY','MULCH','MUMMY','MUNCH','MURAL','MURKY','MUSHY','MUSIC','MUSKY','MUSTY','MYRRH','NADIR','NAIVE','NANNY','NASAL','NASTY','NATAL','NAVAL','NAVEL','NEEDY','NEIGH','NERDY','NERVE','NEVER','NEWER','NEWLY','NICER','NICHE','NIECE','NIGHT','NINJA','NINNY','NINTH','NOBLE','NOBLY','NOISE','NOISY','NOMAD','NOOSE','NORTH','NOSEY','NOTCH','NOVEL','NUDGE','NURSE','NUTTY','NYLON','NYMPH','OAKEN','OBESE','OCCUR','OCEAN','OCTAL','OCTET','ODDER','ODDLY','OFFAL','OFFER','OFTEN','OLDEN','OLDER','OLIVE','OMBRE','OMEGA','ONION','ONSET','OPERA','OPINE','OPIUM','OPTIC','ORBIT','ORDER','ORGAN','OTHER','OTTER','OUGHT','OUNCE','OUTDO','OUTER','OUTGO','OVARY','OVATE','OVERT','OVINE','OVOID','OWING','OWNER','OXIDE','OZONE','PADDY','PAGAN','PAINT','PALER','PALSY','PANEL','PANIC','PANSY','PAPAL','PAPER','PARER','PARKA','PARRY','PARSE','PARTY','PASTA','PASTE','PASTY','PATCH','PATIO','PATSY','PATTY','PAUSE','PAYEE','PAYER','PEACE','PEACH','PEARL','PECAN','PEDAL','PENAL','PENCE','PENNE','PENNY','PERCH','PERIL','PERKY','PESKY','PESTO','PETAL','PETTY','PHASE','PHONE','PHONY','PHOTO','PIANO','PICKY','PIECE','PIETY','PIGGY','PILOT','PINCH','PINEY','PINKY','PINTO','PIPER','PIQUE','PITCH','PITHY','PIVOT','PIXEL','PIXIE','PIZZA','PLACE','PLAID','PLAIN','PLAIT','PLANE','PLANK','PLANT','PLATE','PLAZA','PLEAD','PLEAT','PLIED','PLIER','PLUCK','PLUMB','PLUME','PLUMP','PLUNK','PLUSH','POESY','POINT','POISE','POKER','POLAR','POLKA','POLYP','POOCH','POPPY','PORCH','POSER','POSIT','POSSE','POUCH','POUND','POUTY','POWER','PRANK','PRAWN','PREEN','PRESS','PRICE','PRICK','PRIDE','PRIED','PRIME','PRIMO','PRINT','PRIOR','PRISM','PRIVY','PRIZE','PROBE','PRONE','PRONG','PROOF','PROSE','PROUD','PROVE','PROWL','PROXY','PRUDE','PRUNE','PSALM','PUBIC','PUDGY','PUFFY','PULPY','PULSE','PUNCH','PUPAL','PUPIL','PUPPY','PUREE','PURER','PURGE','PURSE','PUSHY','PUTTY','PYGMY','QUACK','QUAIL','QUAKE','QUALM','QUARK','QUART','QUASH','QUASI','QUEEN','QUEER','QUELL','QUERY','QUEST','QUEUE','QUICK','QUIET','QUILL','QUILT','QUIRK','QUITE','QUOTA','QUOTE','QUOTH','RABBI','RABID','RACER','RADAR','RADII','RADIO','RAINY','RAISE','RAJAH','RALLY','RALPH','RAMEN','RANCH','RANDY','RANGE','RAPID','RARER','RASPY','RATIO','RATTY','RAVEN','RAYON','RAZOR','REACH','REACT','READY','REALM','REARM','REBAR','REBEL','REBUS','REBUT','RECAP','RECUR','RECUT','REEDY','REFER','REFIT','REGAL','REHAB','REIGN','RELAX','RELAY','RELIC','REMIT','RENAL','RENEW','REPAY','REPEL','REPLY','RERUN','RESET','RESIN','RETCH','RETRO','RETRY','REUSE','REVEL','REVUE','RHINO','RHYME','RIDER','RIDGE','RIFLE','RIGHT','RIGID','RIGOR','RINSE','RIPEN','RIPER','RISEN','RISER','RISKY','RIVAL','RIVER','RIVET','ROACH','ROAST','ROBIN','ROBOT','ROCKY','RODEO','ROGER','ROGUE','ROOMY','ROOST','ROTOR','ROUGE','ROUGH','ROUND','ROUSE','ROUTE','ROVER','ROWDY','ROWER','ROYAL','RUDDY','RUDER','RUGBY','RULER','RUMBA','RUMOR','RUPEE','RURAL','RUSTY','SADLY','SAFER','SAINT','SALAD','SALLY','SALON','SALSA','SALTY','SALVE','SALVO','SANDY','SANER','SAPPY','SASSY','SATIN','SATYR','SAUCE','SAUCY','SAUNA','SAUTE','SAVOR','SAVOY','SAVVY','SCALD','SCALE','SCALP','SCALY','SCAMP','SCANT','SCARE','SCARF','SCARY','SCENE','SCENT','SCION','SCOFF','SCOLD','SCONE','SCOOP','SCOPE','SCORE','SCORN','SCOUR','SCOUT','SCOWL','SCRAM','SCRAP','SCREE','SCREW','SCRUB','SCRUM','SCUBA','SEDAN','SEEDY','SEGUE','SEIZE','SEMEN','SENSE','SEPIA','SERIF','SERUM','SERVE','SETUP','SEVEN','SEVER','SEWER','SHACK','SHADE','SHADY','SHAFT','SHAKE','SHAKY','SHALE','SHALL','SHALT','SHAME','SHANK','SHAPE','SHARD','SHARE','SHARK','SHARP','SHAVE','SHAWL','SHEAR','SHEEN','SHEEP','SHEER','SHEET','SHEIK','SHELF','SHELL','SHIED','SHIFT','SHINE','SHINY','SHIRE','SHIRK','SHIRT','SHOAL','SHOCK','SHONE','SHOOK','SHOOT','SHORE','SHORN','SHORT','SHOUT','SHOVE','SHOWN','SHOWY','SHREW','SHRUB','SHRUG','SHUCK','SHUNT','SHUSH','SHYLY','SIEGE','SIEVE','SIGHT','SIGMA','SILKY','SILLY','SINCE','SINEW','SINGE','SIREN','SISSY','SIXTH','SIXTY','SKATE','SKIER','SKIFF','SKILL','SKIMP','SKIRT','SKULK','SKULL','SKUNK','SLACK','SLAIN','SLANG','SLANT','SLASH','SLATE','SLAVE','SLEEK','SLEEP','SLEET','SLEPT','SLICE','SLICK','SLIDE','SLIME','SLIMY','SLING','SLINK','SLOOP','SLOPE','SLOSH','SLOTH','SLUMP','SLUNG','SLUNK','SLURP','SLUSH','SLYLY','SMACK','SMALL','SMART','SMASH','SMEAR','SMELL','SMELT','SMILE','SMIRK','SMITE','SMITH','SMOCK','SMOKE','SMOKY','SMOTE','SNACK','SNAIL','SNAKE','SNAKY','SNARE','SNARL','SNEAK','SNEER','SNIDE','SNIFF','SNIPE','SNOOP','SNORE','SNORT','SNOUT','SNOWY','SNUCK','SNUFF','SOAPY','SOBER','SOGGY','SOLAR','SOLID','SOLVE','SONAR','SONIC','SOOTH','SOOTY','SORRY','SOUND','SOUTH','SOWER','SPACE','SPADE','SPANK','SPARE','SPARK','SPASM','SPAWN','SPEAK','SPEAR','SPECK','SPEED','SPELL','SPELT','SPEND','SPENT','SPERM','SPICE','SPICY','SPIED','SPIEL','SPIKE','SPIKY','SPILL','SPILT','SPINE','SPINY','SPIRE','SPITE','SPLAT','SPLIT','SPOIL','SPOKE','SPOOF','SPOOK','SPOOL','SPOON','SPORE','SPORT','SPOUT','SPRAY','SPREE','SPRIG','SPUNK','SPURN','SPURT','SQUAD','SQUAT','SQUIB','STACK','STAFF','STAGE','STAID','STAIN','STAIR','STAKE','STALE','STALK','STALL','STAMP','STAND','STANK','STARE','STARK','START','STASH','STATE','STAVE','STEAD','STEAK','STEAL','STEAM','STEED','STEEL','STEEP','STEER','STEIN','STERN','STICK','STIFF','STILL','STILT','STING','STINK','STINT','STOCK','STOIC','STOKE','STOLE','STOMP','STONE','STONY','STOOD','STOOL','STOOP','STORE','STORK','STORM','STORY','STOUT','STOVE','STRAP','STRAW','STRAY','STRIP','STRUT','STUCK','STUDY','STUFF','STUMP','STUNG','STUNK','STUNT','STYLE','SUAVE','SUGAR','SUING','SUITE','SULKY','SULLY','SUMAC','SUNNY','SUPER','SURER','SURGE','SURLY','SUSHI','SWAMI','SWAMP','SWARM','SWASH','SWATH','SWEAR','SWEAT','SWEEP','SWEET','SWELL','SWEPT','SWIFT','SWILL','SWINE','SWING','SWIRL','SWISH','SWOON','SWOOP','SWORD','SWORE','SWORN','SWUNG','SYNOD','SYRUP','TABBY','TABLE','TABOO','TACIT','TACKY','TAFFY','TAINT','TAKEN','TAKER','TALLY','TALON','TAMER','TANGO','TANGY','TAPER','TAPIR','TARDY','TAROT','TASTE','TASTY','TATTY','TAUNT','TAWNY','TEACH','TEARY','TEASE','TEDDY','TEETH','TEMPO','TENET','TENOR','TENSE','TENTH','TEPEE','TEPID','TERRA','TERSE','TESTY','THANK','THEFT','THEIR','THEME','THERE','THESE','THETA','THICK','THIEF','THIGH','THING','THINK','THIRD','THONG','THORN','THOSE','THREE','THREW','THROB','THROW','THRUM','THUMB','THUMP','THYME','TIARA','TIBIA','TIDAL','TIGER','TIGHT','TILDE','TIMER','TIMID','TIPSY','TITAN','TITHE','TITLE','TOAST','TODAY','TODDY','TOKEN','TONAL','TONGA','TONIC','TOOTH','TOPAZ','TOPIC','TORCH','TORSO','TORUS','TOTAL','TOTEM','TOUCH','TOUGH','TOWEL','TOWER','TOXIC','TOXIN','TRACE','TRACK','TRACT','TRADE','TRAIL','TRAIN','TRAIT','TRAMP','TRASH','TRAWL','TREAD','TREAT','TREND','TRIAD','TRIAL','TRIBE','TRICE','TRICK','TRIED','TRIPE','TRITE','TROLL','TROOP','TROPE','TROUT','TROVE','TRUCE','TRUCK','TRUER','TRULY','TRUMP','TRUNK','TRUSS','TRUST','TRUTH','TRYST','TUBAL','TUBER','TULIP','TULLE','TUMOR','TUNIC','TURBO','TUTOR','TWANG','TWEAK','TWEED','TWEET','TWICE','TWINE','TWIRL','TWIST','TWIXT','TYING','UDDER','ULCER','ULTRA','UMBRA','UNCLE','UNCUT','UNDER','UNDID','UNDUE','UNFED','UNFIT','UNIFY','UNION','UNITE','UNITY','UNLIT','UNMET','UNSET','UNTIE','UNTIL','UNWED','UNZIP','UPPER','UPSET','URBAN','URINE','USAGE','USHER','USING','USUAL','USURP','UTILE','UTTER','VAGUE','VALET','VALID','VALOR','VALUE','VALVE','VAPID','VAPOR','VAULT','VAUNT','VEGAN','VENOM','VENUE','VERGE','VERSE','VERSO','VERVE','VICAR','VIDEO','VIGIL','VIGOR','VILLA','VINYL','VIOLA','VIPER','VIRAL','VIRUS','VISIT','VISOR','VISTA','VITAL','VIVID','VIXEN','VOCAL','VODKA','VOGUE','VOICE','VOILA','VOMIT','VOTER','VOUCH','VOWEL','VYING','WACKY','WAFER','WAGER','WAGON','WAIST','WAIVE','WALTZ','WARTY','WASTE','WATCH','WATER','WAVER','WAXEN','WEARY','WEAVE','WEDGE','WEEDY','WEIGH','WEIRD','WELCH','WELSH','WENCH','WHACK','WHALE','WHARF','WHEAT','WHEEL','WHELP','WHERE','WHICH','WHIFF','WHILE','WHINE','WHINY','WHIRL','WHISK','WHITE','WHOLE','WHOOP','WHOSE','WIDEN','WIDER','WIDOW','WIDTH','WIELD','WIGHT','WILLY','WIMPY','WINCE','WINCH','WINDY','WISER','WISPY','WITCH','WITTY','WOKEN','WOMAN','WOMEN','WOODY','WOOER','WOOLY','WOOZY','WORDY','WORLD','WORRY','WORSE','WORST','WORTH','WOULD','WOUND','WOVEN','WRACK','WRATH','WREAK','WRECK','WREST','WRING','WRIST','WRITE','WRONG','WROTE','WRUNG','WRYLY','YACHT','YEARN','YEAST','YIELD','YOUNG','YOUTH','ZEBRA','ZESTY','ZONAL'
]

class WordleGame:

    def __init__(self, master, grid_x, grid_y):
        self.master = root
        self.grid_x = grid_x
        self.grid_y = grid_y

        self.colour_bg = "#ffffff"
        self.colour_green = "#538d4e"
        self.colour_yellow = "#b59f3b"
        self.colour_grey = "#3a3a3c"
        self.colour_text = "#ffffff"

        root.title("Wordle - Budget Edition")

    def game_dimensions(self):

        self.button_size = 130
        full_width = self.grid_x * self.button_size + 80
        full_height = self.grid_y * self.button_size + 80
        monitor_centre_x = int(root.winfo_screenwidth() / 2 - full_width / 2)
        monitor_centre_y = int(root.winfo_screenheight() / 2 - full_height / 2)
        root.geometry(f"{full_width}x{full_height}+{monitor_centre_x}+{monitor_centre_y}")
        root.minsize(full_width, full_height)

        self.buttonFont = Font(
            family="Helvetica",
            size=20,
            weight="bold"
        )

        self.winLossFont = Font(
            family="Helvetica",
            size=16,
            weight="bold"
        )

        self.frame = Frame(root, padx=20, pady=20)
        self.frame.place(relx=0.5, rely=0.5, anchor=CENTER)
        # dummy image to size the buttons -- NOT CURRENTLY USING DUE TO NOT BEING ABLE TO ADD TEXT TO BUTTONS
        # self.pixel = PhotoImage(width=1, height=1)

    def game_open(self, wordle_list):

        self.word = np.random.choice(wordle_list)

        self.buttons = []
        self.button_numbers = []
        # used to determine which button in a row has already been changed
        self.fail_state = False
        self.win_state = False
        # becomes [1, 1, 1, 1, 1] with all greens
        self.guess_check = [0, 0, 0, 0, 0]
        self.score = 0
        self.button_offset = 0

        for r in range(self.grid_y):
            for c in range(self.grid_x):
                button_number = r * self.grid_x + c
                self.button_numbers.append(button_number)
                self.buttons.append(Button(self.frame, width=7, height=3,disabledforeground=self.colour_text,\
                                           font=self.buttonFont, bg=f"{self.colour_bg}", state=DISABLED))
                self.buttons[button_number].grid(column=c, row=r+1)

        self.input_field = Entry(self.frame, width=25, font=self.winLossFont)
        self.input_field.grid(row=0, column=0, columnspan=self.grid_x-2, pady=5)
        self.input_field.focus()

        # self.game_label = Label(self.frame, width=20, font=self.winLossFont, text="Choose a Word!", anchor=E, justify="left")
        # self.game_label.grid(row=0, column=1, columnspan=self.grid_x-2, pady=5)

        self.submit_button = Button(self.frame, text="Enter", command=self.game_check_word, font=self.winLossFont)
        self.restart_button = Button(self.frame, text="Restart", command=lambda: self.game_open(wordle_list), font=self.winLossFont)
        # adds a keybind to the master/window, return, which runs thegame_play command (ie lets you submit a guess)
        self.master.bind("<Return>", self.game_check_word)
        self.submit_button.grid(row=0, column=self.grid_x - 2, columnspan=1, pady=5)
        self.restart_button.grid(row=0, column=self.grid_x - 1, columnspan=1, pady=5)

        self.win_button = Button(self.frame, width=12, height=6, text="You won! \n \n Woohoo!", state=DISABLED,\
                                 bg="blue", disabledforeground=self.colour_text, font=self.winLossFont, command=lambda: self.game_open(wordle_list))
        self.loss_button = Button(self.frame, width=12, height=6, text=f"You lost! \n \nThe word was \n \n {self.word}!", state=DISABLED,\
                                 bg="red", disabledforeground=self.colour_text, font=self.winLossFont, command=lambda: self.game_open(wordle_list))
        self.invalid_input = Button(self.frame, width=12, height=6,
                                  text=f"Guess a valid \n5 letter word!", state=DISABLED, \
                                  bg="red", disabledforeground=self.colour_text, font=self.winLossFont,
                                  command=lambda: self.game_open(wordle_list))
        self.not_in_list = Button(self.frame, width=12, height=6,
                                    text=f"Word not in list!\nGuess again.", state=DISABLED, \
                                    bg="#D0D0D0", disabledforeground=self.colour_text, font=self.winLossFont,
                                    command=lambda: self.game_open(wordle_list))


    def game_check_word(self, _event=None):

        # input validation + popup if the user enters an invalid input
        self.word_choice = str(self.input_field.get().upper())
        # removes any existing popups
        self.invalid_input.grid_forget()
        self.not_in_list.grid_forget()
        # checks if the inputs is both 5 characters long AND only composed of letters
        if len(self.word_choice)== 5 and self.word_choice.isalpha() == True:
            # if it's a valid input but not a word in the list
            if self.word_choice not in wordle_list:
                self.not_in_list.grid(row=math.ceil(self.grid_x / 2) - 1, column=int((self.grid_y / 2) - 2),
                                        columnspan=3, \
                                        rowspan=3)
                self.word_choice = None
            # if it is a word in the list
            else:
                self.game_play()
        # if it's not a valid input
        else:
            self.invalid_input.grid(row=math.ceil(self.grid_x / 2) - 1, column=int((self.grid_y / 2) - 2),
                                         columnspan=3, \
                                         rowspan=3)
            self.word_choice = None

    # _event=None -- allows you to click the button to call game_play, as well as hit enter to run it too
    def game_play(self):

        if self.fail_state == True or self.win_state == True:
            return

        # if len(self.word_choice) != self.grid_x or self.word_choice not in wordle_list:
        #     print("Try another word!")
        #     return None

        # deletes the entry field when a guess is made
        self.input_field.delete(0, END)

        # creating an empty dictionary into which we will put the number of repeat letters in the word
        self.repeats_dict = {}
        alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        for letter in alphabet:
            self.repeats_dict[letter] = 0

        # looping through the guess to count how many repeats per letter
        for a in range(self.grid_x):
            self.repeats_dict[self.word_choice[a]] = self.word.count(self.word_choice[a])

        # putting the text on the row
        for a in range(self.grid_x):
            self.buttons[a+self.button_offset].config(text=self.word_choice[a])

        # checking for greens
        for a in range(self.grid_x):
            if self.word_choice[a] == self.word[a] and self.repeats_dict[self.word_choice[a]] != 0:
                self.buttons[a+self.button_offset].config(bg=self.colour_green)
                # marking our guess as 'complete'
                self.repeats_dict[self.word_choice[a]] -= 1
                self.guess_check[a] = 1

        # checking for yellows then greys
        for a in range(self.grid_x):
            if self.guess_check[a] != 1:
                if self.word_choice[a] in self.word and self.repeats_dict[self.word_choice[a]] != 0:
                    self.buttons[a+self.button_offset].config(bg=self.colour_yellow)
                    self.repeats_dict[self.word_choice[a]] -= 1
                elif self.word_choice[a] not in self.word or self.repeats_dict[self.word_choice[a]] == 0:
                    self.buttons[a+self.button_offset].config(bg=self.colour_grey)

        # stating the next function will use the row below
        self.button_offset += self.grid_x

        if self.guess_check == [1, 1, 1, 1, 1]:
            self.win_button.grid(row=math.ceil(self.grid_x/2)-1, column=int((self.grid_y/2)-2), columnspan=3, \
                                 rowspan=3)
            self.input_field.config(state=DISABLED)
            self.submit_button.config(state=DISABLED)
            self.win_state = True
        elif self.button_offset >= max(self.button_numbers):
            self.loss_button.grid(row=math.ceil(self.grid_x / 2)-1, column=int((self.grid_y / 2) - 2), columnspan=3, \
                                 rowspan=3)
            self.input_field.config(state=DISABLED)
            self.submit_button.config(state=DISABLED)
            self.fail_state = True

        # resetting the 'guess check'
        self.guess_check = [0, 0, 0, 0, 0]

root = Tk()

wordle = WordleGame(root, 5, 6)
wordle.game_dimensions()
wordle.game_open(wordle_list)

# restart button should do wordle.game_open(wordle_list)

root.mainloop()