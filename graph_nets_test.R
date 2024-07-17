library(tidyverse)
library(rvest)
library(igraph)
library(ggraph)

nodelist <- readRDS('nodelist.rds')
edgelist <- readRDS('edgelist.rds')
coursenet <- readRDS('coursenet.rds')

# Preparatory Subject Matter
preparatory_subject_matter <- c("POL051", "POL001", "POL002", "POL003", 
                                "POL004", "POL005", "POL007", "POL011A", 
                                "POL011AV", "POL011B", "POL011C", "POL011D", 
                                "POL012A", "POL012B", "POL012Y", "STA013", 
                                "STA013Y", "STA032")

# American Politics
american_politics <- c("POL100", "POL102", "POL104", "POL105", "POL106", 
                       "POL107", "POL108", "POL109", "POL150", "POL151", 
                       "POL152", "POL153", "POL154", "POL155", "POL160", 
                       "POL161", "POL162", "POL163", "POL164", "POL165", 
                       "POL165V", "POL166", "POL168", "POL170", "POL171", 
                       "POL172", "POL174", "POL175", "POL176", "POL180", 
                       "POL183", "POL187", "POL195", "POL196A")

# Comparative Politics
comparative_politics <- c("POL126", "POL140A", "POL140B", "POL140C", 
                          "POL140D", "POL140E", "POL142A", "POL142B", 
                          "POL142C", "POL143A", "POL143B", "POL144A", 
                          "POL144B", "POL146A", "POL146B", "POL147A", 
                          "POL147B", "POL147C", "POL147D", "POL148A", 
                          "POL148B", "POL148C", "POL179", "POL196B")

# International Relations
international_relations <- c("POL120", "POL121", "POL122", "POL123", 
                             "POL124", "POL126", "POL129", "POL130", 
                             "POL131", "POL132", "POL134", "POL135", 
                             "POL136", "POL137", "POL139", "POL190", 
                             "POL196C")

# Political Theory
political_theory <- c("POL110", "POL112", "POL113", "POL114", "POL115", 
                      "POL116", "POL117", "POL118A", "POL118B", "POL118C", 
                      "POL119", "POL187", "POL196D")

pol_all <- unique(c(preparatory_subject_matter, 
             american_politics, 
             comparative_politics, 
             international_relations, 
             political_theory))

pol_subgraph <- induced_subgraph(coursenet, 
                                 vids = V(coursenet)[name %in% pol_all])

pol_graph <- ggraph(pol_subgraph, layout = 'kk') +
   geom_edge_link(color = "black", alpha = .5) +  # Make edges less prominent to focus on nodes
   geom_node_point(aes(color = dept)) +  # Color nodes based on 'dept'
   geom_node_text(aes(label = name),size=3, repel = TRUE) +
   scale_color_viridis_d() +  # Use a discrete color scale, adjust as needed
   labs(title="Political Science Courses") +
   theme_graph() 

animal_science_courses <- c(
   "ANS001", "ANS002", "ANS041", "ANS041L", 
   "BIS002A", "BIS002B", "BIS002C", 
   "CHE002A", "CHE002B", "CHE008A", "CHE008B", "CHE118A", "CHE118B", 
   "MAT016A", "MAT016B", "MAT017A", "MAT017B", "MAT021A", "MAT021B", 
   "PLS120", "STA100", 
   "BIS101", "ANG107", "ABI102", "ABI103", "NPB101", "ANS100", "ANS104",
   "ANS150", "ANS170", "NUT115", "NUT141", 
   "ANS123", "ANS124", "NPB121", "NPB130", 
   "EVE112", "NPB123", "APC100", "WFC120", 
   "AVS100", "NPB117", 
   "ANG111", "ANS106", "ANS133", "ANS134", "ANS135", "ANS136", "ANS137",
   "ANS139", 
   "MCB120L", "MCB160L", "NPB101L", "NPB104L", "PMI126L", 
   "ANS018", "AVS013", "ANS042", "ANS142", "ANS015", "ANS115", "ANS140", 
   "ANS143", "ANS144", "ANS146", "AVS011"
)

ans_subgraph <- induced_subgraph(coursenet, 
                                 vids = V(coursenet)[name %in% animal_science_courses])

ans_graph <- ggraph(ans_subgraph, layout = 'kk') +
   geom_edge_link(color = "black", alpha = .5) +  # Make edges less prominent to focus on nodes
   geom_node_point(aes(color = dept)) +  # Color nodes based on 'dept'
   geom_node_text(aes(label = name),size=3, repel = TRUE) +
   scale_color_viridis_d() +  # Use a discrete color scale, adjust as needed
   labs(title="Animal Science Courses") +
   theme_graph() 


environmental_policy_courses <- c(
   "UWP101", "UWP101V", "UWP101Y", "UWP102G", "UWP104A", "UWP104AV", "UWP104AY", "UWP104B",
   "CMN001", "CMN003", "CMN003V", "CMN003Y", "DRA010",
   "BIS002A", "BIS002B", "BIS010",
   "CHE002A", "CHE010",
   "PHY001A",
   "BIS002C", "CHE002B", "PHY001B",
   "CMN012Y", "SOC012Y", "POL012Y", "PSC012Y", "PLS021", "PLS021V",
   "ECN001A", "ECN001AV", "ECN001AY", "ECN001B", "ECN001BV",
   "ANS001", "ATM060", "AVS013", "GEL001", "HYD010", "PLS012", "SAS012", "SSC010", "WFC051",
   "ESP001",
   "MAT016A", "MAT017A", "MAT019A", "MAT021A", "MAT016B", "MAT017B", "MAT019B", "MAT021B",
   "POL001", "POL001Y",
   "STA013", "STA013Y", "STA032",
   "ESP110", "ESP160", "ESP168A", "ESP168B", "ESP161", "ESP178", "ESP179",
   "SOC106", "STA100", "STA103", "STA108",
   "ARE100A", "ECN100A",
   "ABT150", "LDA150", "ESP106",
   "ARE176", "ECN125", "ESP175"
)


epap_subgraph <- induced_subgraph(coursenet, 
                                 vids = V(coursenet)[name %in% environmental_policy_courses])

epap_graph<- ggraph(epap_subgraph, layout = 'kk') +
   geom_edge_link(color = "black", alpha = .5) +  # Make edges less prominent to focus on nodes
   geom_node_point(aes(color = dept)) +  # Color nodes based on 'dept'
   geom_node_text(aes(label = name),size=3, repel = TRUE) +
   scale_color_viridis_d() +  # Use a discrete color scale, adjust as needed
   labs(title="Environmental Policy Courses") +
   theme_graph() 

environmental_science_courses <- c(
   "UWP101", "UWP101V", "UWP101Y", "UWP102B", "UWP102G", "UWP104E", 
   "CMN001", "CMN003", "CMN003V", "CMN003Y", "DRA010", 
   "BIS002A", "BIS002B", "BIS002C", 
   "GEL001", "GEL050", 
   "CHE002A", "CHE002B", "CHE002AH", "CHE002BH", "CHE002C", "CHE002CH", 
   "PHY001A", "PHY001B", "PHY007A", "PHY007B", "PHY007C", 
   "ECN001A", "ECN001AV", "ECN001AY", 
   "MAT016A", "MAT017A", "MAT021A", "MAT016B", "MAT017B", "MAT021B", 
   "ESP001", 
   "ESM120", 
   "ESP100", "EVE101", 
   "ESP162", 
   "STA100", "STA013", "STA013Y", "STA032", 
   "ATM124", "ESM108", "ESP151L", "ESP179", 
   "ABT150", "LDA150", "ESP106", 
   "ESM092", "ESP092", "ESM192", "ESP192", 
   "ESM195", "ESM194H", 
   "ATM060", "ATM116", "ESM131", "ATM110", "ATM115", "ATM133", "ATM160", "GEL108", "ESM100", "ESM121", "ESP116N", "ESP152", "HYD141", "HYD143", "SSC100", "ENH160", "ESM141", "ESM144", "ESP124", "ESP150C", "ESP151", "ESP155", "EVE115", "EVE117", "EVE147", "EVE149", "GEL136", "PLS101", "PLS130", "PLS162", "WFC168", "ESP163", "ESP165", "ESP167", "ESP171", "ESP172", "ESP174", "SOC160", "EVE100", "WFC154", "ESP127", "ESP123", "EVE180A", "WFC100", "WFC126", "ENH160L", "PLS147", "PLS147L", "ESP121", "WFC122", "EVE104", "EVE181", "PLB117", "WFC155", "ESP151", "ESP155", "EVE147", "PLS162", "PLS163", "ESM141", "EVE109", "EVE138", "WFC125", "WFC151", "ENT103", "EVE112", "EVE114", "PLB102", "PLB116", "PLB119", "WFC110", "WFC111", "WFC120", "WFC134", "ENT116L", "ESP151L", "ESP155L", "EVE112L", "EVE180B", "WFC110L", "WFC111L", "WFC120L", "WFC134L", "ECS032A", "ABT181N", "ABT182", "ESM185", "ESM186", "ESP106", "ESP106", "ATM120", "ESP121", "HYD143", "PLS123", "WFC122", "STA104", "STA106", "STA108", "STA130A", "STA130B", "STA135", "STA137", "STA141A", "STA141B", "STA142A", "ESM100", "ESM121", "ESM131", "ESP124", "ESP150C", "ESP151", "ESP152", "ESP155", "EVE117", "EVE147", "GEL136", "PLS101", "PLS130", "PLS163", "WFC125", "WFC168", "ESP160", "ESP165", "ESP166", "ESP167", "ESP168A", "ESP169", "ESP170", "ESP171", "ESP172", "ESP173", "ESP174", "SOC160", "ESP161", "HYD150", "ARE106", "ECN102", "STA101", "STA103", "STA106", "STA108", "STA130A", "STA131A", "ENT104", "ESM141", "ESM144", "ESP151", "ESP155", "EVE115", "PLB117", "PLS130", "WFC110", "WFC111", "WFC120", "WFC134", "ATM116", "ATM133", "ESP116N", "ESP152", "HYD143", "ESM185", "ESM186", "SSC100", "SSC100", "ESM100", "HYD134", "SSC102", "SSC105", "SSC107", "SSC109", "SSC111", "SSC112", "SSC118", "SSC120", "ESM121", "ESP165", "ESP166", "ESP171", "ESP172", "ESP174", "ESP179", "ESM185", "GEL134", "HYD147", "SSC118", "ATM160", "ESP116N", "ESP150A", "ESP150C", "ESP151", "ESP155", "EVE117", "GEL132", "PLS130", "SSC100", "ESM100", "ESM121", "ESM125", "HYD150", "ATM133", "HYD110", "HYD118", "ESP116N", "HYD124", "HYD143", "HYD144", "HYD145", "ESP151L", "ESP152", "GEL035", "GEL109", "GEL136", "GEL140", "ABT181N", "ABT182", "ESP106", "ESM185", "ESP166", "ESP168A", "ESP169", "ESP172", "ESP173", "ESP174", "ESP179", "SSC105", "SSC118", "SSC120", "ESP151", "ESP155", "EVE115", "WFC120", "WFC134"
)

esm_subgraph <- induced_subgraph(coursenet, 
                                 vids = V(coursenet)[name %in% environmental_science_courses])

esm_graph <- ggraph(esm_subgraph, layout='kk') +
   geom_edge_link(color = "black", alpha = .5) +  # Make edges less prominent to focus on nodes
   geom_node_point(aes(color = dept)) +  # Color nodes based on 'dept'
   geom_node_text(aes(label = name),size=3, repel = TRUE) +
   scale_color_viridis_d() +  # Use a discrete color scale, adjust as needed
   labs(title="Environmental Science Courses") +
   theme_graph() 

design_courses <- c(
   "DES001", "DES014", "DES021", "DES015", "DES016", 
   "UWP011", "UWP012", "UWP048", "UWP049", 
   "DES040A", "DES040B", "DES040C", "DES040D", 
   "ART012", "DES050", "DES051", "DES070", "DES077", 
   "AHI100", "AHI101", "AHI102", "AHI110", "AHI120A", "AHI122", "AHI121", "AHI123", "AHI130", "AHI148", "AHI150", "AHI151", "AHI152", "AHI153", "AHI154", "AHI155", "AHI156", "AHI157", "AHI158", "AHI163A", "AHI163B", "AHI163C", "AHI163D", "AHI164", "AHI168", "AHI172A", "AHI172B", "AHI173", "AHI175", "AHI176A", "AHI176B", "AHI176C", "AHI177", "AHI178B", "AHI178C", "AHI179B", "AHI180", "AHI181", "AHI182", "AHI183A", "AHI183B", "AHI183C", "AHI184", "AHI185", "AHI186", "AHI187", "AHI188A", "AHI188B", "AHI188C", "AHI189", 
   "DES127A", "DES128A", "DES138", "DES141", "DES142A", "DES142B", "DES143", "DES144", "DES145", "DES146", "DES148", "DES149", 
   "DRA114", "DRA116", "DRA150", "DRA155", "CDM155", "CDM159", "TCS155", "TCS159", 
   "DES107", "DES111", "DES112", "DES113", "DES115", "DES116", "DES117", "DES126", "DES127B", "DES128B", "DES131", "DES132A", "DES134A", "DES134B", "DES135A", "DES135B", "DES136A", "DES136B", "DES137A", "DES137B", "DES150", "DES151", "DES155A", "DES156", "DES157A", "DES158", "DES160", "DES161", "DES165", "DES166", "DES167", "DES168", "DES169", "DES170", "DES171", "DES175", "DES177", "DES178", "DES180A", "DES185", "DES186", "DES191A", "DES191B", "DES191C", "DES191D", 
   "ART110A", "ART110B", "ART113", "ART114A", "CDM100", "CDM104", "CDM125", "CDM130", "CDM131", "CHI172", "DRA124A", "DRA124B", "DRA124C", "DRA124D", "DRA124E", "DRA128", "DRA130", "DRA170", "LDA141", 
   "DES154", "DES159", "DES157B", "DES179", "DES180B", "DES187"
)

design_subgraph <- induced_subgraph(coursenet, 
                                    vids = V(coursenet)[name %in% design_courses])

des_graph <- ggraph(design_subgraph, layout = 'kk') +
   geom_edge_link(color = "black", alpha = .5) +  # Make edges less prominent to focus on nodes
   geom_node_point(aes(color = dept)) +  # Color nodes based on 'dept'
   geom_node_text(aes(label = name),size=3, repel = TRUE) +
   scale_color_viridis_d() +  # Use a discrete color scale, adjust as needed
   labs(title="Design Courses") +
   theme_graph()

communication_courses <- c(
   "ANT004", "LIN001", "LIN001Y", 
   "CMN010Y", "CMN010V", "CMN001", "CMN003", "CMN003V", "CMN003Y", "CMN005", 
   "PHI012", "PSC001", "PSC001V", "PSC001Y", "SOC001", "STA013", "STA013Y", "SOC056", "SOC056Y", 
   "CMN101", "CMN101Y", "CMN102", "CMN102V", "CMN120", "CMN120V", "CMN140", 
   "CMN170", "CMN170V", "CMN172", "CMN110", "CMN111", "CMN112", "CMN114", "CMN121", "CMN122", "CMN123", 
   "CMN124", "CMN130", "CMN131", "CMN132", "CMN136", "CMN136V", "CMN139", "CMN141", "CMN142", "CMN143", 
   "CMN144", "CMN145", "CMN146", "CMN147", "CMN147V", "CMN148", "CMN150V", "CMN151", "CMN161", "CMN165", 
   "CMN170", "CMN170V", "CMN172", "CMN174", "CMN176", "CMN176V", "CMN178", "CMN180", "CMN189A", "CMN189B", 
   "CMN189C", "CMN189D", "ANT120", "ECN122", "LIN171", "LIN177", "LIN182", "POL165", "POL165V", "PSC100", 
   "PSC100Y", "PSC107", "PSC152", "PSC154", "PSC154V", "SOC126", "STA106", "STA108"
)

comm_subgraph <- induced_subgraph(coursenet, 
                                  vids = V(coursenet)[name %in% communication_courses])

comm_graph <- ggraph(comm_subgraph, layout = 'kk') +
   geom_edge_link(color = "black", alpha = .5) +  # Make edges less prominent to focus on nodes
   geom_node_point(aes(color = dept)) +  # Color nodes based on 'dept'
   geom_node_text(aes(label = name),size=3, repel = TRUE) +
   scale_color_viridis_d() +  # Use a discrete color scale, adjust as needed
   labs(title="Communication Courses") +
   theme_graph()


mechanical_engineering_courses <- c(
   "CMN001", "ENG003", "ENG003Y", 
   "MAT021A", "MAT021B", "MAT021C", "MAT021D", "MAT022A", "MAT022B", 
   "PHY009A", "PHY009B", "PHY009C", 
   "CHE002A", "CHE002AH", "CHE002B", "CHE002BH", 
   "ENG004", "ENG017", "ENG017V", "ENG035", "ENG006", "EME005", "EME050", "ENG045", "ENG045Y", 
   "COM001", "COM002", "COM003", "COM004", "ENL003", "ENL003V", "NAS005", "UWP001", "UWP001V", "UWP001Y", 
   "ENG100", "ENG102", "ENG103", "ENG104", "ENG105", "ENG190", 
   "EME106", "EME108", "EME109", "EME150A", "EME165", "EME172", 
   "EME185A", "EME185B", "EAE130A", "EAE130B", "EAE143A", "EAE143B", 
   "ECH140", "ECI114", "ECS130", "ENG180", "MAT118A", "MAT128A", "MAT128B", "EME115", "EME151", "STA130A", "STA131A", 
   "ENG122", "EME121", "EME139", "EME150B", "EME154", "EME171", 
   "EAE129", "EAE138", "EAE140", "EAE142", "EAE143A", "EAE143B", "ENG188", "EMS180", "EMS182", "EME134", "EME152", "EME161", "EME163", "EME164", 
   "UWP101", "UWP101V", "UWP101Y", "UWP102E", "UWP104A", "UWP104AV", "UWP104AY", "UWP104E", "UWP104T"
)

me_subgraph <- induced_subgraph(coursenet, 
                                vids = V(coursenet)[name %in% mechanical_engineering_courses])

me_graph <- ggraph(me_subgraph, layout = 'kk') +
   geom_edge_link(color = "black", alpha = .5) +  # Make edges less prominent to focus on nodes
   geom_node_point(aes(color = dept)) +  # Color nodes based on 'dept'
   geom_node_text(aes(label = name),size=3, repel = TRUE) +
   scale_color_viridis_d() +  # Use a discrete color scale, adjust as needed
   labs(title="Mechanical Engineering Courses") +
   theme_graph()

russian_courses <- c(
   "RUS001", "RUS002", "RUS003", "RUS004", "RUS005", "RUS006", 
   "RUS101A", "RUS101B", "RUS101C", "RUS102", "RUS103", "RUS105", 
   "RUS120", "RUS122", "RUS124", "RUS126", "RUS129", "RUS130", "RUS133", "RUS139", "RUS140", "RUS141", "RUS142", "RUS143", "RUS150"
)

russian_subgraph <- induced_subgraph(coursenet, 
                                     vids = V(coursenet)[name %in% russian_courses])

russian_graph <- ggraph(russian_subgraph, layout = 'kk') +
   geom_edge_link(color = "black", alpha = .5) +  # Make edges less prominent to focus on nodes
   geom_node_point(aes(color = dept)) +  # Color nodes based on 'dept'
   geom_node_text(aes(label = name),size=3, repel = TRUE) +
   scale_color_viridis_d() +  # Use a discrete color scale, adjust as needed
   labs(title="Russian Courses") +
   theme_graph()

dir.create("graphs", showWarnings = FALSE)

ggsave("graphs/pol_graph.png", pol_graph, width = 10, height = 5, units = "in", dpi = 300)
ggsave("graphs/ans_graph.png", ans_graph, width = 10, height = 5, units = "in", dpi = 300)
ggsave("graphs/epap_graph.png", epap_graph, width = 10, height = 5, units = "in", dpi = 300)
ggsave("graphs/esm_graph.png", esm_graph, width = 10, height = 5, units = "in", dpi = 300)
ggsave("graphs/des_graph.png", des_graph, width = 10, height = 5, units = "in", dpi = 300)
ggsave("graphs/comm_graph.png", comm_graph, width = 10, height = 5, units = "in", dpi = 300)
ggsave("graphs/me_graph.png", me_graph, width = 10, height = 5, units = "in", dpi = 300)
ggsave("graphs/russian_graph.png", russian_graph, width = 10, height = 5, units = "in", dpi = 300)

################################################################################

coursenet_graph <- ggraph(coursenet, layout = 'kk') +
   geom_edge_link(color = "black", alpha = .5) +  # Make edges less prominent to focus on nodes
   geom_node_point(aes(color = dept)) +  # Color nodes based on 'dept'
   scale_color_viridis_d() +  # Use a discrete color scale, adjust as needed
   labs(title="CourseNet Supreme")+
   theme_graph() +
   theme(legend.position = "none")

ggsave("graphs/coursenet_graph.png", coursenet_graph, width = 10, height = 10, units = "in", dpi = 300)

# Computing centrality measures for each vertex
V(coursenet)$indegree   <- degree(coursenet, mode = "in")
V(coursenet)$outdegree  <- degree(coursenet, mode = "out")
V(coursenet)$closeness  <- closeness(coursenet, mode = "total")
V(coursenet)$betweeness <- betweenness(coursenet, normalized = TRUE)

# Extracting each vectex features as a data.frame
stats <- as_data_frame(coursenet, what = "vertices")

# Computing quantiles for each variable
stats_degree <- cbind(
   indegree   = quantile(stats$indegree, c(.025, .5, .975), na.rm = TRUE),
   outdegree  = quantile(stats$outdegree, c(.025, .5, .975), na.rm = TRUE),
   closeness  = quantile(stats$closeness, c(.025, .5, .975), na.rm = TRUE),
   betweeness = quantile(stats$betweeness, c(.025, .5, .975), na.rm = TRUE)
)

stats_degree

stats %>% 
   group_by(dept) %>% 
   summarise(mean_out = median(outdegree),
             mean_in = median(indegree),
             mean_between = median(betweeness),
             mean_close = median(closeness)) %>% 
   arrange(desc(mean_in))
