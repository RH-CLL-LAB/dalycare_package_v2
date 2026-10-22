clean_SDS_pato = function(data){
  #' @title
  #' clean_SDS_pato
  #' @author
  #' christian brieghel; catrine karlson
  #' @description
  #' Cleans and merges pathology data
  #' @examples
  #' SDS_pato_clean = SDS_fctrekvisition %>% clean_SDS_pato()
  #' @references
  #' Github

  reqs = data$dw_ek_rekvisition %>% unique
  load_dataset('SDS_dimpatologiskdiagnose', reqs, 'dw_ek_rekvisition')

  data %>%
    transmute(patientid,
              shak = ifelse(is.na(rekvirerende_afd_shak_kode), rekvirerende_yder_shak_kode, rekvirerende_afd_shak_kode),
              sor = rekvirerende_afd_sor_kode,
              date_biopsy = dato_rekvirering,
              dw_ek_rekvisition) %>%
    left_join(SDS_dimpatologiskdiagnose_subset %>%
                arrange(dw_ek_rekvisition, materialenummer, diagnose_snomed_sekvensnummer) %>%
                transmute(dw_ek_rekvisition,
                          material_number = materialenummer,
                          snomed_number = str_pad(diagnose_snomed_sekvensnummer, 3, pad = "0"),
                          snomed = diagnose_snomed_kode), 'dw_ek_rekvisition') %>%
    group_by(dw_ek_rekvisition) %>%
    mutate(n_material = max(material_number)) %>%
    ungroup() %>%
    group_by(dw_ek_rekvisition, material_number) %>%
    mutate(n_snomed = n()) %>%
    ungroup() %>%
    select(everything(), snomed) %>%
    left_join(Codes_SNOMED %>%
                select(snomed = c_snomedkode, text = Text) %>%
                group_by(snomed) %>%
                slice(1) %>%
                ungroup(), 'snomed') %>%

    group_by(patientid, material_number, dw_ek_rekvisition) %>% # date_biopsy
    mutate(material_type = ifelse(snomed_number == '001', text, NA)) %>%
    arrange(patientid,  dw_ek_rekvisition, material_number, snomed_number) %>%
    fill(material_type) %>%
    ungroup() %>%
    left_join(Codes_SHAK_long %>% select(shak = kode, region, hospital, clinic), 'shak') %>%
    # sor_name: name of the ordering unit from Codes_sor (SOR-kode). The sor code is
    # converted to text without scientific notation, since it is a 15-16 digit number.
    mutate(.sor_key = if(is.character(sor)) trimws(sor) else format(sor, scientific = FALSE, trim = TRUE)) %>%
    left_join(Codes_sor %>%
                transmute(.sor_key = `SOR-kode`, sor_name = Enhedsnavn) %>%
                distinct(.sor_key, .keep_all = TRUE), '.sor_key') %>%
    select(-.sor_key) %>%
    arrange(patientid, date_biopsy, material_number) %>%
    distinct()
}
