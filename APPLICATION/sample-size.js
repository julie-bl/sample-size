// JavaScript - Utilise uniquement les écouteurs Shiny pour la navigation des onglets
$(document).ready(function() {
    
    // Attendre que Shiny soit chargé
    setTimeout(function() {
        setupAllRadioButtons();
        setupTabNavigation();
    }, 1000);
    
    // Configuration de la navigation par onglets avec Shiny
    function setupTabNavigation() {
        
        // Écouter les clics sur les éléments de menu pour déclencher la navigation Shiny
        $('.sidebar-menu .treeview > a').on('click', function(e) {
            var $treeview = $(this).closest('.treeview');
            var menuId = $treeview.attr('id');
            var menuText = $(this).find('span').text().trim();
            
            
            // Déterminer le tabName à activer selon le menu cliqué
            var targetTab = '';
            switch(menuId) {
                case 'DPid':
                    targetTab = 'hiddenChartsDP';
                    break;
                case 'CMid':
                    targetTab = 'hiddenChartsMean';
                    break;
                case 'CPid':
                    targetTab = 'hiddenChartsProp';
                    break;
                case 'BEPid':
                    targetTab = 'hiddenChartsBEP';
                    break;
            }
            
            if (targetTab) {
                // Utiliser Shiny pour changer d'onglet
                Shiny.setInputValue('sidebarID', targetTab);
            }
        });
            }
    
    // Fonction principale d'initialisation pour tous les boutons radio (CONSERVÉE)
    function setupAllRadioButtons() {
        
        // Traiter chaque groupe de boutons radio dans la sidebar ET le contenu principal
        $('.main-sidebar .shiny-input-radiogroup, .sidebar-menu .shiny-input-radiogroup, .content .shiny-input-radiogroup').each(function() {
            var $group = $(this);
            
            // Transformer les labels en boutons cliquables (sauf les questions qui ont control-label)
            $group.find('label:not(.control-label)').each(function() {
                var $label = $(this);
                $label.css('cursor', 'pointer');
            });
        });
        
        // Mettre à jour l'état visuel initial
        updateAllRadioButtons();
    }
    
    // Fonction pour mettre à jour l'état visuel de tous les boutons radio (CONSERVÉE)
    function updateAllRadioButtons() {
        
        $('.main-sidebar .shiny-input-radiogroup, .sidebar-menu .shiny-input-radiogroup, .content .shiny-input-radiogroup').each(function() {
            var $group = $(this);
            
            // Enlever la classe selected de tous les labels (sauf ceux avec control-label)
            $group.find('label:not(.control-label)').removeClass('radio-selected');
            
            // Trouver le bouton sélectionné et appliquer le style
            var $selectedInput = $group.find('input[type="radio"]:checked');
            if ($selectedInput.length > 0) {
                var selectedValue = $selectedInput.val();
                
                // Trouver le label correspondant à cette valeur (exclure les labels avec control-label)
                $group.find('label:not(.control-label)').each(function() {
                    var $label = $(this);
                    var labelText = $label.text().trim();
                    
                    if (labelText === selectedValue) {
                        $label.addClass('radio-selected');
                        
                        // Forcer les styles avec JavaScript
               
                        $label.css({
                            'background-color': '#ea5b0c',
                            'color': 'white',
                            'border-color': '#ea5b0c',
                            'font-weight': '600'
                        });
                    }
                });
            }
        });
    }
    
    // Gestionnaire de clic sur tous les labels (CONSERVÉ)
    $(document).on('click', '.main-sidebar .shiny-input-radiogroup label:not(.control-label), .sidebar-menu .shiny-input-radiogroup label:not(.control-label), .content .shiny-input-radiogroup label:not(.control-label)', function(e) {
        e.preventDefault();
        e.stopPropagation();
        
        var $clickedLabel = $(this);
        var clickedValue = $clickedLabel.text().trim();
        var $group = $clickedLabel.closest('.shiny-input-radiogroup');
        var $targetInput = $group.find('input[type="radio"]').filter(function() {
            return $(this).val() === clickedValue;
        });
        
        if ($targetInput.length > 0) {
            
            // Décocher tous les autres boutons du même groupe
            var inputName = $targetInput.attr('name');
            $('input[name="' + inputName + '"]').prop('checked', false);
            
            // Activer le bouton radio cliqué
            $targetInput.prop('checked', true);
            
            // Déclencher l'événement change pour informer Shiny
            $targetInput.trigger('change');
            
            // Mettre à jour l'affichage immédiatement
            updateAllRadioButtons();
        }
    });
    
    
    // Écouter les changements sur tous les inputs radio pour Shiny
    $(document).on('change', '.main-sidebar .shiny-input-radiogroup input[type="radio"], .sidebar-menu .shiny-input-radiogroup input[type="radio"], .content .shiny-input-radiogroup input[type="radio"]', function() {
        setTimeout(function() {
            updateAllRadioButtons();
        }, 10);
    });
    
});
