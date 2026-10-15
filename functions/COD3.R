COD3  = function(data){
  #' @title
  #' COD3
  #' @author
  #' christian brieghel
  #' @description
  #' Groups cause of death (COD) ICD10 codes into meaningful groups. 
  #' @note 
  #' We recommend using COD2() or COD3() over COD(). 
  #' COD2 prioritizes infectious death from either cod a, b, c, or d
  #' COD3 prioritizes infectious death from any cause of death
  #' @examples
  #' all_causes_of_death %>%  COD3()
  #' @references 
  #' https://www.krebsdaten.de/Krebs/EN/Content/Methods/Coding_manual/coding_manual_node.html        
  #' Rotbain et al. Leukemia. 2021;35(9):2570-2580. Table S2
  cat('Rotbain et al. Leukemia. 2021;35(9):2570-2580.')
  death.malign.hem = paste0('^D' , c('C81','C82','C83','C84','C85','C86','C87','C88','C89',
                       'C90','C91','C92','C93','C94','C95','C96',
                       'D45','D46','D47'))
  
  death.malign.oth = paste0('^D' , c('C00','C01','C02','CO3','C04','C05','C06','C07','C08','C09',
                                    'C10','C11','C12','C13','C14','C15','C16','C17','C18','C19', 
                                    'C20','C21','C22','C23','C24','C25','C26','C27','C28','C29',
                                    'C30','C31','C32','C33','C34','C35','C36','C37','C38','C39',
                                    'C40','C41','C42','C43','C44','C45','C46','C47','C48','C49',
                                    'C50','C51','C52','C53','C54','C55','C56','C57','C58','C59',
                                    'C60','C61','C66','C63','C64','C65','C66','C67','C68','C69',
                                    'C70','C71','C72','C73','C74','C75','C76','C77','C78','C79',
                                    'C80','C97'))
  
  death.inf = paste0('^D' , c('A0','A1','A2','A3','A4','A5','A6','A7','A8','A9',
                              'B0','B1','B2','B3','B4','B5','B6','B7','B8','B9', 
                              'D733','E060','E321', 
                              'G00','G01','G02','G038','G039','G04','G05','G06','G07','G08','G09',
                              'H00','H010','H03','H043','H050','H061','H100','H105','H106','H107','H108','H109','H130','H131','H150','H151','H190','H191','H192','H200','H220','H320','H440','H441','H600','H601','H603','H620','H621','H622','H623','H624','H660','H664','H670','H671','H700','H750',
                              'I301','I320','I321','I330','I339','I400','I410', 'I411', 'I412','I430','I681','I980',
                              'I981', 'J0','J1','J20','J21','J22','J36','J390','J391','J65','J851','J852','J853','J854','J855','J856','J857','J858','J859','J86',
                              'K046','K047','K052','K113','K122','K230','K35','K570','K572','K574','K578','K61','K630','K650','K659','K67','K750','K770','K810','K871',
                              'L00','L01','L02','L03','L04','L05','L06','L07','L08',
                              'M00','M01','M600','M608','M630','M631','M632','M860','M861','M868','M869',
                              'N00','N01','N080','N10','N151','N160','N290','N291','N300','N308','N33','N340','N370','N390','N410','N412','N431','N45','N481','N482','N492','N499','N51','N61','N700','N710','N72','N730','N733','N740','N741','N742','N743','N744','N751','N752','N753','N754','N755','N756','N757','N758','N760','N762','N764','N768','N770','N771',
                              'O23','O753','O85','O86','O91','O98',
                              'R650','R651',
                              'T802','T814')) 
  
  death.chf = paste0('^D' , c('I01','I020', 'I05', 'I06', 'I07', 'I08', 'I09', 'I10', 'I11', 'I13', 'I20', 'I21', 'I22', 'I23', 'I24', 'I25', 'I26', 'I27', 'I28', 
                              'I302', 'I303', 'I304', 'I305', 'I306', 'I307', 'I308', 'I309', 'I31', 'I321', 'I322', 'I323', 'I324', 'I325', 'I326', 'I327', 'I328', 'I329', 'I331', 'I332',  'I333', 'I334', 'I335', 'I336', 'I337', 'I338', 'I401', 'I402', 'I403', 'I404', 'I405', 'I406', 'I407', 'I408', 'I409', 'I413', 'I414', 'I415', 'I416', 'I417', 'I418', 'I419', 'I42', 'I431', 'I432', 'I433', 'I434', 'I435', 'I436', 'I438', 'I439', 'I45', 'I46', 'I47', 'I48', 'I49', 'I51', 'I52', 
                              'I70', 'I71', 'I72', 'I731', 'I738', 'I739', 'I74', 'I771', 'I790', 'I792'))    
  
  death.cvd = paste0('^D' , c( 'H340', 'DT817Y1', # TIA+port-OP infarkt
                               'I6', 'I725', 'I726', 'G450', 'G451', 'G452', 'G453', 'G458',
                               'G459', 'G46', 'G041', 'G114', 'G801', 'G802', 'G81', 'G820',
                               'G821', 'G822'))
  death.respiratory = paste0('^D' , c('J23','J24','J25','J26','J27','J28','J29',
                                      'J30','J31','J32','J33','J34','J35',   'J37','J38',
                                      'J392','J393','J394','J395','J396','J397','J398','J399',
                                      'J4','J5', 
                                      'J60','J61','J62','J63','J64', 'J66','J67','J68','J69', 
                                      'J7', 
                                      'J80','J81','J82','J83','J84','J87','J88','J89',
                                      'J90','J92','J93','J94','J95',
                                      'J961','J962','J963','J964','J965','J966','J967','J968',# ER 20/4-26: excluding DJ969, DJ960, Respirationsinsufficiens
                                      'J97','J98','J99'))
  
  
  # assign death causes: CLL related death = 1, CLL unrelated death = 2
  data %>% 
    mutate(class_of_death = ifelse(str_detect(diagnosis, str_flatten(death.cvd, '|' )), 'Cerebrovascular', 'Other'),
           class_of_death = ifelse(str_detect(diagnosis, str_flatten(death.chf, '|' )), 'Cardiovascular', class_of_death),
           class_of_death = ifelse(str_detect(diagnosis, str_flatten(death.malign.oth, '|' )), 'Solid cancer', class_of_death),
           class_of_death = ifelse(str_detect(diagnosis, str_flatten(death.malign.hem, '|' )), 'Hematological cancer', class_of_death),
           class_of_death = ifelse(str_detect(diagnosis, str_flatten(death.inf, '|' )), 'Infectious', class_of_death)) %>%
    mutate(class_of_death_underlying = ifelse(cause_of_death_short=='Underlying', class_of_death, NA),
           class_of_death_underlying = ifelse(class_of_death == 'Infectious', class_of_death, class_of_death_underlying),
           class_of_death_underlying = factor(class_of_death_underlying, 
                                              c('Infectious', 'Hematological cancer', 'Solid cancer', 'Cardiovascular', 'Cerebrovascular', 'Other'))) %>% 
    mutate(class_of_death_7 = ifelse(str_detect(diagnosis, str_flatten(death.respiratory, '|' )), 'Respiratory', class_of_death),
           class_of_death_7 = factor(class_of_death_7, c('Infectious', 'Hematological cancer', 'Solid cancer', 'Cardiovascular', 'Cerebrovascular', 'Respiratory', 'Other')),
           class_of_death = factor(class_of_death, c('Infectious', 'Hematological cancer', 'Solid cancer', 'Cardiovascular', 'Cerebrovascular', 'Other')))
  
}
