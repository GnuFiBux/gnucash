/************************************************************************\
 * dialog-tax-info-editor.c                                             *
 *                                                                      *
 * Copyright 2013 Carsten Rinke (carsten.rinke@gmx.de)                  *
 * dialog-tax-info-editor.c                                             *
 * This program is free software; you can redistribute it and/or modify *
 * it under the terms of the GNU General Public License as published by *
 * the Free Software Foundation; either version 2 of the License, or    *
 * (at your option) any later version.                                  *
 *                                                                      *
 * This program is distributed in the hope that it will be useful,      *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of       *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the        *
 * GNU General Public License for more details.                         *
 *                                                                      *
 * You should have received a copy of the GNU General Public License    *
 * along with this program; if not, write to the Free Software          *
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,           *
 * MA 02110-1301, USA.                                                  *
\************************************************************************/

#include "config.h"

#include <gtk/gtk.h>
#include <glib/gi18n.h>

#include "dialog-utils.h"
#include "gnc-tree-view-account.h"
#include "gnc-prefs.h"
#include "gnc-component-manager.h"

#define DIALOG_TAX_INFO_EDITOR_CM_CLASS "dialog-tax-info-editor"
#define GNC_PREFS_GROUP "dialogs.tax-info-editor"

enum
{
    TAX_COUNTRY,
    TAX_TYPE,
    TAX_FORM,
    TAX_ITEM,
    TAX_COLUMN,
    N_TAX_COMPONENTS
};

/**********************************************************\
 * here we keep the context of the tax info editor dialog *
\**********************************************************/
typedef struct
{
    GtkWidget *window;

    GtkComboBox *combobox_tax_countries;
    GtkComboBox *combobox_tax_types;
    GtkComboBox *combobox_tax_years;

    GtkWidget *tree_view_forms;
    GtkWidget *tree_view_lines;
    GtkWidget *tree_view_columns;
    GtkWidget *tree_view_accounts_assigned;
    GtkWidget *tree_view_account_pool;

    GtkTreeSelection *form_selection;
    GtkTreeSelection *line_selection;
    GtkTreeSelection *column_selection;
    GtkTreeSelection *accounts_assigned_selection;
    GtkTreeSelection *account_pool_selection;
    GtkMenu *account_pool_popup_menu;
    GtkMenu *accounts_assigned_popup_menu;

    GtkWidget *tax_item_heading;
    GtkWidget *tax_item_description;

    gchar* tax_locale_string;
    gchar* tax_type_string;
    gchar* tax_form_string;
    gchar* tax_item_ID_string;
    gchar* tax_cell_ID_string;

    gboolean lookup_selection;

} TaxInfoEditor;

/**************************************************************************\
 * Prototypes                                                             *
\**************************************************************************/

gint show_info_dialog (GtkWidget *parent, guint info_type, const gchar *message);
void clear_tax_item_forms (TaxInfoEditor *ti_editor);
void clear_tax_item_lines (TaxInfoEditor *ti_editor);
void clear_tax_item_columns (TaxInfoEditor *ti_editor);
void clear_tax_item_assigned_accounts (TaxInfoEditor *ti_editor);
void clear_tax_item_heading_and_description (TaxInfoEditor *ti_editor);
gboolean filter_column_single_occurance (GtkListStore *store, gchar* tax_item_id);
GtkBuilder* init_builder (gchar *object_name, gchar *file_name);
static gboolean account_filter_isNotAssigned (Account *account, gpointer data);
static gboolean account_filter_isAssigned (Account *account, gpointer data);
static gboolean account_filter_hasCurrentTaxID (Account *account, gpointer data);
GtkTreePath* find_path_of_string_in_model_column (GtkTreeModel *data_store, guint column, gchar *target);
gint find_index_of_string_in_model_column (GtkTreeModel *data_store, guint column, gchar *target);
void account_lookup_cb (GtkTreeSelection *selection, gpointer data);
static gint account_tree_popup_handler (GtkWidget *widget, GdkEvent *event);
void account_pool_changed_cb (GtkTreeSelection *selection, gpointer data);
void account_assigned_activated_cb (GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, gpointer data);
void account_unassign_cb (GtkMenuItem *menuitem, gpointer data);
void account_pool_activated_cb (GtkTreeView *tree_view, GtkTreePath *path, GtkTreeViewColumn *column, gpointer data);
void account_assign_cb (GtkMenuItem *menuitem, gpointer data);
void tax_column_selection_changed_cb (GtkTreeSelection *selection, gpointer data);
void tax_line_selection_changed_cb (GtkTreeSelection *selection, gpointer data);
void tax_form_selection_changed_cb (GtkTreeSelection *selection, gpointer data);
void tax_type_changed_cb (GtkComboBox *combobox_tax_types, gpointer data);
void tax_year_changed_cb (GtkComboBox *combobox_tax_years, gpointer data);
void tax_country_changed_cb (GtkComboBox *combobox_tax_countries, gpointer data);
void tax_info_editor_response_handler (GtkDialog *dialog, gint response, gpointer user_data);
void tax_info_editor_close_handler (GtkDialog *dialog, gpointer user_data);
void gnc_tax_info_editor_create (GtkWidget *parent, TaxInfoEditor *ti_editor);
void gnc_tax_info_editor (GtkWidget *parent);
void unassign_account (Account *account);
GList* check_and_repair_all (Account *account, GtkWidget *parent);
gint show_decision_dialog (GtkWidget *parent, guint info_type, const gchar *message);

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

gint
show_info_dialog (GtkWidget *parent, guint info_type, const gchar *message)
{
    GtkWidget *info_dialog;
    gint response;
    info_dialog = gtk_message_dialog_new (GTK_WINDOW (parent),
                                          GTK_DIALOG_DESTROY_WITH_PARENT,
                                          info_type,
                                          GTK_BUTTONS_CLOSE,
                                          message,
                                          NULL, NULL);
    response = gtk_dialog_run (GTK_DIALOG (info_dialog));
    gtk_widget_destroy (info_dialog);

    return response;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

gint
show_decision_dialog (GtkWidget *parent,
                      guint info_type,
                      const gchar *message)
{
    GtkWidget *info_dialog, *label;
    gint choice;

    label = gtk_label_new (message);

    info_dialog = gtk_dialog_new_with_buttons (
                      "Check and Repair",
                      GTK_WINDOW (parent),
                      GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT,
                      GTK_STOCK_DISCARD,
                      GTK_RESPONSE_REJECT,
                      GTK_STOCK_OK,
                      GTK_RESPONSE_ACCEPT,
                      NULL);

    gtk_container_add (
        GTK_CONTAINER (gtk_dialog_get_content_area (GTK_DIALOG (info_dialog))),
        label);

    gtk_widget_show_all (info_dialog);

    choice = gtk_dialog_run (GTK_DIALOG (info_dialog));
    gtk_widget_destroy (info_dialog);

    return choice;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

void
clear_tax_item_forms (TaxInfoEditor *ti_editor)
{
    GtkTreeViewColumn *column;

    column = gtk_tree_view_get_column
                (GTK_TREE_VIEW (ti_editor->tree_view_forms),
                 0);

    if (column)
    {
        gtk_tree_view_remove_column (GTK_TREE_VIEW (ti_editor->tree_view_forms),
                                     column);
        gtk_tree_selection_unselect_all (ti_editor->form_selection);
    }

    return;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

void
clear_tax_item_lines (TaxInfoEditor *ti_editor)
{
    GtkTreeViewColumn *column;

    column = gtk_tree_view_get_column
                 (GTK_TREE_VIEW(ti_editor->tree_view_lines),
                  0);

    if (column)
    {
        gtk_tree_view_remove_column (GTK_TREE_VIEW (ti_editor->tree_view_lines),
                                     column);
        gtk_tree_selection_unselect_all (ti_editor->line_selection);
    }

    return;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

void
clear_tax_item_columns (TaxInfoEditor *ti_editor)
{
    GtkTreeViewColumn *column;

    column = gtk_tree_view_get_column
                 (GTK_TREE_VIEW (ti_editor->tree_view_columns),
                  0);
    if (column)
    {
        gtk_tree_view_remove_column
            (GTK_TREE_VIEW (ti_editor->tree_view_columns),
             column);
        gtk_tree_selection_unselect_all (ti_editor->column_selection);
    }

    return;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

void
clear_tax_item_assigned_accounts (TaxInfoEditor *ti_editor)
{
    GtkTreeViewColumn *column;

    gnc_tree_view_account_refilter
        (GNC_TREE_VIEW_ACCOUNT (ti_editor->tree_view_accounts_assigned));

    gnc_tree_view_account_refilter
        (GNC_TREE_VIEW_ACCOUNT (ti_editor->tree_view_account_pool));

    gtk_tree_view_expand_all
        (GTK_TREE_VIEW (ti_editor->tree_view_accounts_assigned));

    return;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

void
clear_tax_item_heading_and_description (TaxInfoEditor *ti_editor)
{
    gtk_label_set_text (GTK_LABEL (ti_editor->tax_item_description),
                        N_("No line selected."));
    gtk_label_set_text (GTK_LABEL (ti_editor->tax_item_heading),
                        N_("No line selected."));

    return;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

gboolean
filter_column_single_occurance (GtkListStore *store, gchar* tax_item_id)
{
    GtkTreeIter iter;
    gchar *tax_item_id2;

    /* get the first row */
    if (gtk_tree_model_get_iter_first (GTK_TREE_MODEL(store), &iter))
    {
        /* do the filter */
        do
        {
            /* get the tax item ID from the current row */
            gtk_tree_model_get (GTK_TREE_MODEL(store),
                                &iter, 0, &tax_item_id2, -1);
            if (g_strcmp0 (tax_item_id, tax_item_id2))
            {
                gtk_list_store_remove (store, &iter);
                return TRUE;
            }
        }
        while (gtk_tree_model_iter_next (GTK_TREE_MODEL (store), &iter));
    }
    return FALSE;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

GtkBuilder*
init_builder (gchar *object_name, gchar *file_name)
{
    GtkBuilder *builder;
    gchar *error_message;

    builder = gtk_builder_new ();

    if (!gnc_builder_add_from_file (builder, file_name, object_name))
    {
        error_message = g_strdup_printf ("%s\n%s\n%s\n%s\n",
                              N_("Error: Cannot load object"),
                              object_name,
                              N_("from file"),
                              file_name);
        show_info_dialog (NULL,
                          GTK_MESSAGE_ERROR,
                          error_message);

        g_free (file_name);
        g_free (object_name);
        g_free (error_message);
        g_object_unref (builder);
        return NULL;
    }
    g_free (file_name);

    return builder;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

static gboolean
account_filter_isNotAssigned (Account *account, gpointer data)
{
    if (xaccAccountGetTaxRelated (account))
    {
        return FALSE;
    }
    return TRUE;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

static gboolean
account_filter_isAssigned (Account *account, gpointer data)
{
    if (xaccAccountGetTaxRelated (account))
    {
        return TRUE;
    }
    if (gnc_account_n_descendants (account) > 0)
    {
      GList *list_of_descendents=NULL;

      list_of_descendents = gnc_account_get_descendants (account);

      while (list_of_descendents)
      {
		  if (xaccAccountGetTaxRelated ((Account*)list_of_descendents->data))
		  {
			  g_list_free(list_of_descendents);
			  return TRUE;
		  }
		  list_of_descendents = list_of_descendents->next;
	  }
    }
    return FALSE;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

static gboolean
account_filter_hasCurrentTaxID (Account *account, gpointer data)
{
    TaxInfoEditor *ti_editor = data;

    if (xaccAccountGetTaxRelated (account))
    {
        if (!g_strcmp0(xaccAccountGetTaxUSCode (account),
                       ti_editor->tax_cell_ID_string))
        {
            return TRUE;
		}
    }
    if (gnc_account_n_descendants (account) > 0)
    {
        GList *list_of_descendents=NULL;

        list_of_descendents = gnc_account_get_descendants (account);

        while (list_of_descendents)
        {
		    if (xaccAccountGetTaxRelated ((Account*)list_of_descendents->data))
		    {
                if (!g_strcmp0 (
			          xaccAccountGetTaxUSCode (
			              (Account*)list_of_descendents->data),
                          ti_editor->tax_cell_ID_string))
                {
					g_list_free(list_of_descendents);
					return TRUE;
				}
		    }
		    list_of_descendents = list_of_descendents->next;
	    }
	    g_list_free(list_of_descendents);
    }
    return FALSE;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

GtkTreePath*
find_path_of_string_in_model_column (GtkTreeModel *data_store,
                                     guint column, gchar *target)
{
    GtkTreeIter iter;
    gboolean valid;
    GtkTreePath *path;
    gchar *path_string;
    gchar *data_string;

    valid = gtk_tree_model_get_iter_first (data_store, &iter);
    while (valid)
    {
        gtk_tree_model_get (data_store, &iter,
                            column, &data_string,
                            -1);

        if (g_strcmp0 (data_string, target))
        {
            valid = gtk_tree_model_iter_next (data_store, &iter);
        }
        else
        {
            path = gtk_tree_model_get_path (data_store, &iter);
            return path;
        }
    }
    return NULL;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

gint
find_index_of_string_in_model_column (GtkTreeModel *data_store,
                                      guint column, gchar *target)
{
    GtkTreePath *path;
    gchar *path_string;
    gint index;

    path = find_path_of_string_in_model_column (data_store, column, target);
    if (path)
    {
        path_string = gtk_tree_path_to_string (path);
        index = (gint) g_ascii_strtoll (path_string, NULL, 0);
        return index;
    }
    return -1;
}

/**************************************************************************\
 * Local helper function                                                  *
\**************************************************************************/

GList*
check_and_repair_all (Account *account,
                      GtkWidget *parent)
{
	Account *root_account;
	GList *account_list;
	GList *list_iter;

	/*******************************************\
	 * get a list of all accounts in this book *
	\*******************************************/

	root_account = gnc_account_get_root (account);

	if (!root_account)
	{
        show_info_dialog (parent,
                          GTK_MESSAGE_INFO,
                          N_("No further unsupported Tax ID found."));
        return NULL;
	}

	account_list = gnc_account_get_descendants (root_account);

	/**************************************************************\
	 * remove all elements that do not hold a tax related account *
	\**************************************************************/

	list_iter = account_list;

	while (list_iter)
	{
		if (!xaccAccountGetTaxRelated ((Account*)list_iter->data))
		{
		    account_list = g_list_remove (account_list,list_iter->data);
	        list_iter = account_list;
		}
		else
		{
		    list_iter = g_list_next (list_iter);
		}
	}

	if (!account_list)
	{
        show_info_dialog (parent,
                          GTK_MESSAGE_INFO,
                          N_("No further unsupported Tax ID."));
        return NULL;
	}

	/*******************************************************************\
	 * remove all elements that hold a tax account with invalid tax ID *
	\*******************************************************************/

	list_iter = account_list;

	while (list_iter)
	{
        if (g_strv_length (
               g_strsplit_set (
                   xaccAccountGetTaxUSCode (list_iter->data),
                   ":",
                   -1))
            != N_TAX_COMPONENTS)
        {
            switch (show_decision_dialog (
                        parent,
                        GTK_MESSAGE_INFO,
                        g_strdup_printf ("%s\n%s\n\n%s",
                                         N_("Unsupported Tax ID:"),
                                         xaccAccountGetTaxUSCode (list_iter->data),
                                         N_("Unassign this Tax ID?"))))
            {
				case GTK_RESPONSE_ACCEPT:
                    unassign_account (list_iter->data);
				break;

				default:
				break;
	        }
		    account_list = g_list_remove (account_list,list_iter->data);
	        list_iter = account_list;
		}
		else
		{
		    list_iter = g_list_next (list_iter);
		}
	}

	if (!account_list)
	{
        show_info_dialog (parent,
                          GTK_MESSAGE_INFO,
                          N_("No further unsupported Tax ID left."));
        return NULL;
	}

	return account_list;
}

/****************************************************************************\
 * account_lookup_cb                                                        *
 * Look up an account an locate him in the tax declaration                  *
 *                                                                          *
 * Args:   selection pointer - the selection that is sending the signal     *
 *         gpointer data  - pointer to the tax info editor dialog context   *
 * Return: nothing                                                          *
\****************************************************************************/

void
account_lookup_cb (GtkTreeSelection *selection, gpointer data)
{
    TaxInfoEditor *ti_editor = data;
    GtkBuilder *builder;
    GtkWidget *dialog = NULL;
    GtkWidget *tree_view;
    GtkTreeSelection *lookup_selection;
    GList *accounts;
    GList *accounts_tmp;
    GList *node;
    const gchar *str;
    gchar **tax_item_ID_string_array;
    guint i;
    GtkTreePath *path;
    GtkTreeIter iter;

    builder = gtk_builder_new();

    if (selection)
    {
        /*************************************\
         * load the dialog window definition *
        \*************************************/

        gnc_builder_add_from_file (builder,
                               "dialog-tax-info-editor.glade",
                               "LookupWindow");
        dialog = GTK_WIDGET(gtk_builder_get_object (builder, "LookupWindow"));

        /*********************************************************************\
         * add an account tree with all accounts that have a tax ID assigned *
        \*********************************************************************/

        tree_view = GTK_WIDGET (gnc_tree_view_account_new (FALSE));
        lookup_selection = gtk_tree_view_get_selection (GTK_TREE_VIEW (tree_view));
        gtk_tree_selection_set_mode (lookup_selection, GTK_SELECTION_SINGLE);
        gtk_container_add
            (GTK_CONTAINER
                (GTK_WIDGET (gtk_builder_get_object (builder, "scrolledwindow6"))),
            tree_view);
        gnc_tree_view_account_set_filter (GNC_TREE_VIEW_ACCOUNT(tree_view),
                                      account_filter_isAssigned,
                                      NULL, NULL);

        /******************\
         * run the dialog *
        \******************/

        gtk_dialog_run (GTK_DIALOG (dialog));
        accounts = gnc_tree_view_account_get_selected_accounts
                   (GNC_TREE_VIEW_ACCOUNT (tree_view));
    }
    else
    {
        show_info_dialog (dialog,
                          GTK_MESSAGE_INFO,
                          N_("This is a call without a selection."));
        return;
    }

    /**************************************************\
     * evaluate the account selection from the dialog *
    \**************************************************/

    while (accounts)
    {
        /**************************************************\
         * split the tax ID string up into its components *
         * - tax country                                  *
         * - tax type                                     *
         * - tax form                                     *
         * - tax line                                     *
         * - tax column                                   *
        \**************************************************/
        str = xaccAccountGetTaxUSCode (accounts->data);
        tax_item_ID_string_array = g_strsplit_set (str, ":", -1);

        if (g_strv_length (tax_item_ID_string_array) != N_TAX_COMPONENTS)
        {
            switch (show_decision_dialog (
                        dialog,
                        GTK_MESSAGE_INFO,
                        g_strdup_printf ("%s\n%s\n\n%s",
                                         N_("Unsupported Tax ID:"),
                                         str,
                                         N_("Unassign this Tax ID?"))))
            {
				case GTK_RESPONSE_ACCEPT:
                    unassign_account (accounts->data);
                    gnc_tree_view_account_refilter
                        (GNC_TREE_VIEW_ACCOUNT(
                            ti_editor->tree_view_account_pool));
				break;

				default:
				break;
	        }

            switch (show_decision_dialog (
                        dialog,
                        GTK_MESSAGE_INFO,
                        g_strdup_printf ("%s\n%s",
                          N_("Check for and unassign all unsupported TAX IDs?"),
                          N_("(You will be prompted to confirm for each.)"))))
            {
				case GTK_RESPONSE_ACCEPT:
                    accounts_tmp = check_and_repair_all (accounts->data, dialog);
                    g_list_free(accounts);
                    accounts = accounts_tmp;
				break;

				default:
				break;
	        }
	        break; /* restart the loop with new accounts list */
        }
        else
        {
            /*********************************************************\
             * adjust the tax country combox according to tax locale *
            \*********************************************************/

            i = find_index_of_string_in_model_column
                (
                    GTK_TREE_MODEL
                        (gtk_combo_box_get_model
                            (ti_editor->combobox_tax_countries)),
                    1, /* string column */
                    tax_item_ID_string_array[TAX_COUNTRY] /* search string */
                );

            if (i < 0)
				switch (show_decision_dialog (
                        dialog,
                        GTK_MESSAGE_INFO,
                        g_strdup_printf ("%s\n%s\n\n%s",
                                         N_("Unsupported Tax Country:"),
                                         str,
                                         N_("Unassign this Tax ID?"))))
				{
					case GTK_RESPONSE_ACCEPT:
						unassign_account (accounts->data);
						gnc_tree_view_account_refilter
							(GNC_TREE_VIEW_ACCOUNT(
								ti_editor->tree_view_account_pool));
					break;

					default:
					break;
				}
            else
            {
                gtk_combo_box_set_active(ti_editor->combobox_tax_countries, i);

                /***********************************************************\
                 * adjust the tax type combox according to tax type string *
                \***********************************************************/

                i = find_index_of_string_in_model_column
                    (
                        GTK_TREE_MODEL
                            (gtk_combo_box_get_model
                                (ti_editor->combobox_tax_types)),
                        0, /* string column */
                        tax_item_ID_string_array[TAX_TYPE] /* search string */
                    );

                if (i < 0)
					switch (show_decision_dialog (
                        dialog,
                        GTK_MESSAGE_INFO,
                        g_strdup_printf ("%s\n%s\n\n%s",
                                         N_("Unsupported Tax Type:"),
                                         str,
                                         N_("Unassign this Tax ID?"))))
					{
						case GTK_RESPONSE_ACCEPT:
							unassign_account (accounts->data);
							gnc_tree_view_account_refilter
								(GNC_TREE_VIEW_ACCOUNT(
									ti_editor->tree_view_account_pool));
						break;

						default:
						break;
					}
                else
                {
                    gtk_combo_box_set_active(ti_editor->combobox_tax_types, i);

                    /********************************************************\
                     * choose any of the tax years -> simply take the first *
                    \********************************************************/
                    gtk_combo_box_set_active(ti_editor->combobox_tax_years, 0);

                    /****************************************\
                     * adjust the selecton of the tax form  *
                    \****************************************/

                    path = find_path_of_string_in_model_column
                           (gtk_tree_view_get_model (
                               GTK_TREE_VIEW (ti_editor->tree_view_forms)),
                            0, /* string column */
                            tax_item_ID_string_array[TAX_FORM]
                                /* search string */
                           );

                    if (!path)
						switch (show_decision_dialog (
							dialog,
							GTK_MESSAGE_INFO,
							g_strdup_printf ("%s\n%s\n\n%s",
                                         N_("Unsupported Tax Form:"),
                                         str,
                                         N_("Unassign this Tax ID?"))))
						{
							case GTK_RESPONSE_ACCEPT:
								unassign_account (accounts->data);
								gnc_tree_view_account_refilter
									(GNC_TREE_VIEW_ACCOUNT(
										ti_editor->tree_view_account_pool));
							break;

							default:
							break;
						}
                    else
                    {
                        gtk_tree_selection_select_path
                            (ti_editor->form_selection, path);
                        gtk_tree_path_free (path);
                        path = NULL;

                        /***************************************\
                         * adjust the selecton of the tax line *
                        \***************************************/

                        path = find_path_of_string_in_model_column
                               (gtk_tree_view_get_model (
                                   GTK_TREE_VIEW (ti_editor->tree_view_lines)),
                                0, /* string column */
                                tax_item_ID_string_array[TAX_ITEM]
                                   /* search string */
                               );

                        if (!path)
							switch (show_decision_dialog (
								dialog,
								GTK_MESSAGE_INFO,
								g_strdup_printf ("%s\n%s\n\n%s",
                                         N_("Unsupported Tax Line:"),
                                         str,
                                         N_("Unassign this Tax ID?"))))
							{
								case GTK_RESPONSE_ACCEPT:
									unassign_account (accounts->data);
									gnc_tree_view_account_refilter
									(GNC_TREE_VIEW_ACCOUNT(
										ti_editor->tree_view_account_pool));
								break;

								default:
								break;
							}
                        else
                        {
                            gtk_tree_selection_select_path
                                (ti_editor->line_selection, path);
                            gtk_tree_path_free (path);
                            path = NULL;

                            /*****************************************\
                             * adjust the selecton of the tax column *
                            \*****************************************/

                            path = find_path_of_string_in_model_column
                                   (gtk_tree_view_get_model (
                                       GTK_TREE_VIEW (
                                           ti_editor->tree_view_columns)),
                                    2, /* string column */
                                    tax_item_ID_string_array[TAX_COLUMN]
                                        /* search string */
                                   );

                            if (!path)
								switch (show_decision_dialog (
									dialog,
									GTK_MESSAGE_INFO,
									g_strdup_printf ("%s\n%s\n\n%s",
                                         N_("Unsupported Tax Column:"),
                                         str,
                                         N_("Unassign this Tax ID?"))))
								{
									case GTK_RESPONSE_ACCEPT:
									unassign_account (accounts->data);
									gnc_tree_view_account_refilter
									(GNC_TREE_VIEW_ACCOUNT(
										ti_editor->tree_view_account_pool));
									break;

									default:
									break;
								}
                            else
                            {
                                gtk_tree_selection_select_path
                                    (ti_editor->column_selection, path);
                                gtk_tree_path_free(path);
                                path = NULL;
                            }
                        } /* end of update tax column */
                    } /* end of update tax item line */
                } /* end of update tax form */
            } /* end of update tax type */
        } /* end of update tax country */

        g_strfreev(tax_item_ID_string_array);
        accounts = g_list_next (accounts);
    }

    gtk_widget_destroy (dialog);
    g_object_unref (builder);

    return;
}

/****************************************************************************\
 * account_pool_popup_handler                                               *
 * Offer the options to either assign the selected account or to go to the  *
 * look up function                                                         *
 *                                                                          *
 * Args:   pop up widget - the widget defining the displayed menue items    *
 *         gdk event - holding the event type                               *
 * Return: nothing                                                          *
\****************************************************************************/

static gint
account_tree_popup_handler (GtkWidget *widget, GdkEvent *event)
{
    GtkMenu *menu;
    GdkEventButton *event_button;

    g_return_val_if_fail (widget != NULL, FALSE);
    g_return_val_if_fail (GTK_IS_MENU (widget), FALSE);
    g_return_val_if_fail (event != NULL, FALSE);

    /* The "widget" is the menu that was supplied when
     * g_signal_connect_swapped() was called.
     */

    menu = GTK_MENU (widget);

    if (event->type == GDK_BUTTON_PRESS)
    {
        event_button = (GdkEventButton *) event;
        if (event_button->button == 3)
        {
            gtk_menu_popup (menu, NULL, NULL, NULL, NULL,
                            event_button->button, event_button->time);
            return TRUE;
        }
    }
    return FALSE;
}

/**************************************************************************\
 * this is not needed  -  but left here as a place holder for future use  *
\**************************************************************************

void
account_pool_changed_cb (GtkTreeSelection *selection, gpointer data)
{
  printf("Account pool single click.\n");
}
*/

/****************************************************************************\
 * account_assigned_activated_cb                                            *
 * Unassign the tax cell ID from the accounts selected in the accounts tree *
 *                                                                          *
 * Args:   selection pointer - the selection that is sending the signal     *
 *         gpointer data  - pointer to the tax info editor dialog context   *
 * Return: nothing                                                          *
\****************************************************************************/


void
unassign_account (Account *account)
{
    xaccAccountBeginEdit (account);
    xaccAccountSetTaxRelated (account, FALSE);
    xaccAccountSetTaxUSCode (account, "");
    xaccAccountCommitEdit (account);
}

void
account_assigned_activated_cb (GtkTreeView       *tree_view,
                               GtkTreePath       *path,
                               GtkTreeViewColumn *column,
                               gpointer           data)
{
    TaxInfoEditor *ti_editor = data;
    GList *accounts;
    GList *node;

    accounts = gnc_tree_view_account_get_selected_accounts
               (GNC_TREE_VIEW_ACCOUNT (ti_editor->tree_view_accounts_assigned));

    for (node = accounts; node; node = node->next)
    {
        /* unassign the tax cell ID */
        Account *account = node->data;

        xaccAccountBeginEdit (account);

        xaccAccountSetTaxRelated (account, FALSE);
        xaccAccountSetTaxUSCode (account, "");
        xaccAccountCommitEdit (account);
    }

    gnc_tree_view_account_refilter
        (GNC_TREE_VIEW_ACCOUNT(ti_editor->tree_view_accounts_assigned));

    gnc_tree_view_account_refilter
        (GNC_TREE_VIEW_ACCOUNT(ti_editor->tree_view_account_pool));

}

void
account_unassign_cb (GtkMenuItem *menuitem, gpointer data)
{
    account_assigned_activated_cb (NULL, NULL, NULL, data);
    return;
}

/**************************************************************************\
 * account_pool_activated_cb                                              *
 * Assign the tax cell ID to the accounts selected in the accounts tree   *
 *                                                                        *
 * Args:   selection pointer - the selection that is sending the signal   *
 *         gpointer data  - pointer to the tax info editor dialog context *
 * Return: nothing                                                        *
\**************************************************************************/

void
account_pool_activated_cb (GtkTreeView       *tree_view,
                           GtkTreePath       *path,
                           GtkTreeViewColumn *column,
                           gpointer           data)
{
    TaxInfoEditor *ti_editor = data;
    GList *accounts;
    GList *node;

    if (ti_editor->tax_cell_ID_string)
    {
        /* get selected accounts */
        accounts = gnc_tree_view_account_get_selected_accounts
                   (GNC_TREE_VIEW_ACCOUNT (ti_editor->tree_view_account_pool));

        for (node = accounts; node; node = node->next)
        {
            /* assign the tax cell ID */
            Account *account = node->data;

            xaccAccountBeginEdit (account);

            xaccAccountSetTaxRelated (account, TRUE);
            xaccAccountSetTaxUSCode (account, ti_editor->tax_cell_ID_string);
            xaccAccountCommitEdit (account);

            gnc_tree_view_account_expand_to_account (
                GNC_TREE_VIEW_ACCOUNT(ti_editor->tree_view_accounts_assigned),
                account);
        }

        gnc_tree_view_account_refilter
            (GNC_TREE_VIEW_ACCOUNT(ti_editor->tree_view_accounts_assigned));

        gnc_tree_view_account_refilter
            (GNC_TREE_VIEW_ACCOUNT(ti_editor->tree_view_account_pool));
    }
}

void
account_assign_cb (GtkMenuItem *menuitem, gpointer data)
{
    account_pool_activated_cb (NULL, NULL, NULL, data);
    return;
}

/**************************************************************************\
 * tax_column_selection_changed_cb                                        *
 * initialize the dialog widgets                                          *
 *                                                                        *
 * Args:   widget pointer - the widget that is sending the signal         *
 *         gpointer data  - pointer to the tax info editor dialog context *
 * Return: nothing                                                        *
\**************************************************************************/

void
tax_column_selection_changed_cb (GtkTreeSelection *selection, gpointer data)
{
    TaxInfoEditor *ti_editor = data;
    GtkTreeIter iter;
    GtkTreeModel *model;
    gchar *tax_column_id;
    GtkListStore *store;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;

    /*****************************************\
     * clear the elements of the tax cell ID *
    \*****************************************/

    if (ti_editor->tax_cell_ID_string)
    {
        g_free (ti_editor->tax_cell_ID_string);
        ti_editor->tax_cell_ID_string = NULL;
        clear_tax_item_assigned_accounts (ti_editor);
    }

    /***********************************************************\
     * if the tax column list has only been updated or cleared *
     * without new selection then stop here                    *
    \***********************************************************/

    if (!gtk_tree_selection_get_selected (selection, &model, &iter))
        return;
    gtk_tree_model_get (model, &iter, 2, &tax_column_id, -1);

    /*************************************************************\
     * compose the tax cell ID                                   *
     * here cell means the field in the tax form to be filled in *
    \*************************************************************/

    ti_editor->tax_cell_ID_string =
        g_strdup_printf("%s:%s:%s:%s:%s",
                        ti_editor->tax_locale_string,  /* tax country locale */
                        ti_editor->tax_type_string,    /* tax type */
                        ti_editor->tax_form_string,    /* tax form */
                        ti_editor->tax_item_ID_string, /* tax item ID */
                        tax_column_id                  /* tax item column */
                       );

    /****************************************\
     * clear the dependent tax item widgets *
    \****************************************/
    clear_tax_item_assigned_accounts (ti_editor);

    return;
}

/**************************************************************************\
 * tax_line_selection_changed_cb                                          *
 * initialize the dialog widgets                                          *
 *                                                                        *
 * Args:   widget pointer - the widget that is sending the signal         *
 *         gpointer data  - pointer to the tax info editor dialog context *
 * Return: nothing                                                        *
\**************************************************************************/

void
tax_line_selection_changed_cb (GtkTreeSelection *selection, gpointer data)
{
    GtkTreeIter iter;
    GtkTreeModel *model;
    gchar *form, *line;
    gchar* text_selection = NULL;
    gchar* object_name;
    TaxInfoEditor *ti_editor = data;
    GtkListStore *store;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GError *error = NULL;
    GtkBuilder *builder;

    /****************************************\
     * clear the dependent tax item widgets *
    \****************************************/
    clear_tax_item_assigned_accounts (ti_editor);
    clear_tax_item_columns (ti_editor);

    /*****************************************\
     * clear the elements of the tax cell ID *
    \*****************************************/

    if (ti_editor->tax_item_ID_string)
    {
        g_free (ti_editor->tax_item_ID_string);
        ti_editor->tax_item_ID_string = NULL;
    }

    if (ti_editor->tax_cell_ID_string)
    {
        g_free (ti_editor->tax_cell_ID_string);
        ti_editor->tax_cell_ID_string = NULL;
        clear_tax_item_assigned_accounts (ti_editor);
    }

    /***************************************************************\
     * get the current tax item ID from the list of tax item lines *
    \***************************************************************/

    if (!gtk_tree_selection_get_selected (ti_editor->line_selection,
                                          &model, &iter))
        return;

    gtk_tree_model_get (model, &iter, 0, &ti_editor->tax_item_ID_string, -1);

    /********************************************************************\
     * get the current tax item heading from the list of tax item lines *
    \********************************************************************/

    gtk_tree_model_get (model, &iter, 1, &text_selection, -1);
    gtk_label_set_text (GTK_LABEL (ti_editor->tax_item_heading),
                        text_selection);

    /************************************************************************\
     * get the current tax item description from the list of tax item lines *
    \************************************************************************/

    gtk_tree_model_get (model, &iter, 2, &text_selection, -1);
    gtk_label_set_text (GTK_LABEL (ti_editor->tax_item_description),
                        text_selection);

    /*************************************\
     * load the list of tax item columns *
    \*************************************/

    /* compose the name of the tax item columns list
     * for this tax type and this form */

    gtk_combo_box_get_active_iter (ti_editor->combobox_tax_types, &iter);

    gtk_tree_model_get
        (GTK_TREE_MODEL(gtk_combo_box_get_model(ti_editor->combobox_tax_types)),
         &iter, 0, &text_selection, -1);

    if (!gtk_tree_selection_get_selected (ti_editor->form_selection,
                                          &model, &iter))
        return;

    gtk_tree_model_get (model, &iter, 0, &form, -1);

    /********************************************************\
     * load the tax item column list                        *
     * always load the list of columns from the glade file, *
     * because we manipulate it afterwards                  *
    \********************************************************/

    /* get the list of tax lines that belongs to this tax form */
    object_name = g_strdup_printf ("listTaxItemColumns%s%s",
                                   text_selection,
                                   form);
    builder = init_builder
                  (object_name,
                   g_strdup_printf("dialog-tax-info-editor-%s-%s%s.glade",
                                    ti_editor->tax_locale_string,
                                    text_selection,
                                    form));

    if (!builder) return;
    store = GTK_LIST_STORE (gtk_builder_get_object (builder, object_name));
    if (!store)
    {
		char* error_message;
        error_message = g_strdup_printf ("%s %s\n",
                              N_("Error: Cannot load object"),
                              object_name);
        show_info_dialog (NULL,
                          GTK_MESSAGE_ERROR,
                          error_message);
        g_free (object_name);
        g_free (error_message);
		g_object_unref (builder);
		return;
	}
    g_free (object_name);

    /******************************************************************\
     * filter all lines that do not belong to the current tax item ID *
    \******************************************************************/

    while (filter_column_single_occurance (store,
                                           ti_editor->tax_item_ID_string));

    /***********************************\
     * display the list of tax columns *
    \***********************************/

    gtk_tree_view_set_model(GTK_TREE_VIEW(ti_editor->tree_view_columns),
                            GTK_TREE_MODEL(store));

    renderer = gtk_cell_renderer_text_new ();

    column = gtk_tree_view_column_new_with_attributes
                 (N_("Column"), /* column heading */
                  renderer, "text",
                  2, /* column */
                  NULL);

    gtk_tree_view_append_column (GTK_TREE_VIEW (ti_editor->tree_view_columns),
                                 column);

    g_free (form);
    g_object_unref (builder);

    return;
}

/**************************************************************************\
 * tax_form_selection_changed_cb                                          *
 * initialize the dialog widgets                                          *
 *                                                                        *
 * Args:   widget pointer - the widget that is sending the signal         *
 *         gpointer data  - pointer to the tax info editor dialog context *
 * Return: nothing                                                        *
\**************************************************************************/

void
tax_form_selection_changed_cb (GtkTreeSelection *selection, gpointer data)
{
    GtkTreeIter iter;
    GtkTreeModel *model;
    gchar* text_selection = NULL;
    gchar* object_name;
    TaxInfoEditor *ti_editor = data;
    GtkListStore *store = NULL;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GError *error = NULL;
    gint column_tax_year;
    GtkBuilder *builder;

    if (!gtk_tree_selection_get_selected (selection, &model, &iter))
        return;

    /****************************************\
     * clear the dependent tax item widgets *
    \****************************************/

    clear_tax_item_heading_and_description (ti_editor);
    clear_tax_item_columns (ti_editor);
    clear_tax_item_lines (ti_editor);

    /*****************************************\
     * clear the elements of the tax cell ID *
    \*****************************************/

    if (ti_editor->tax_form_string)
    {
        g_free (ti_editor->tax_form_string);
        ti_editor->tax_form_string = NULL;
    }
    if (ti_editor->tax_item_ID_string)
    {
        g_free (ti_editor->tax_item_ID_string);
        ti_editor->tax_item_ID_string = NULL;
    }
    if (ti_editor->tax_cell_ID_string)
    {
        g_free (ti_editor->tax_cell_ID_string);
        ti_editor->tax_cell_ID_string = NULL;
        clear_tax_item_assigned_accounts (ti_editor);
    }

    /*****************************************\
     * compose the name of the tax line list *
     * for this tax type and this tax form   *
    \*****************************************/

    gtk_tree_model_get (model, &iter, 0, &ti_editor->tax_form_string, -1);

    gtk_combo_box_get_active_iter
        (GTK_COMBO_BOX(ti_editor->combobox_tax_types),
         &iter);

    gtk_tree_model_get
        (GTK_TREE_MODEL
            (gtk_combo_box_get_model
                (GTK_COMBO_BOX(ti_editor->combobox_tax_types))),
         &iter, 0, &text_selection, -1);

    /**********************************************\
     * set the store pointer to the tax line list *
    \**********************************************/

    /* get the list of tax lines that belongs to this tax form */
    object_name = g_strdup_printf("listTaxItemLines%s%s",
                                  text_selection,
                                  ti_editor->tax_form_string);

    builder = init_builder
                  (object_name,
                   g_strdup_printf("dialog-tax-info-editor-%s-%s%s.glade",
                                    ti_editor->tax_locale_string,
                                    text_selection,
                                    ti_editor->tax_form_string));
    if (!builder) return;

    store = GTK_LIST_STORE (gtk_builder_get_object (builder, object_name));
    if (!store)
    {
		char* error_message;
        error_message = g_strdup_printf ("%s %s\n",
                              N_("Error: Cannot load object"),
                              object_name);
        show_info_dialog (NULL,
                          GTK_MESSAGE_ERROR,
                          error_message);
        g_free (object_name);
        g_free (error_message);
		g_object_unref (builder);
		return;
	}
    g_free (object_name);

    /**************************************************************\
     * select the column from the store according to the tax year *
    \**************************************************************/

    column_tax_year = gtk_combo_box_get_active
                          (GTK_COMBO_BOX(ti_editor->combobox_tax_years)) + 3;

    /*********************************\
     * display the list of tax lines *
    \*********************************/

    gtk_tree_view_set_model(GTK_TREE_VIEW(ti_editor->tree_view_lines),
                            GTK_TREE_MODEL(store));

    renderer = gtk_cell_renderer_text_new ();

    column = gtk_tree_view_column_new_with_attributes
                 (N_("Line"), /* column heading */
                  renderer, "text",
                  column_tax_year, /* column */
                  NULL);

    gtk_tree_view_append_column (GTK_TREE_VIEW (ti_editor->tree_view_lines),
                                 column);

    g_object_unref (builder);

    return;
}

/**************************************************************************\
 * tax_type_changed_cb                                                    *
 * initialize the dialog widgets                                          *
 *                                                                        *
 * Args:   widget pointer - the widget that is sending the signal         *
 *         gpointer data  - pointer to the tax info editor dialog context *
 * Return: nothing                                                        *
\**************************************************************************/

void
tax_type_changed_cb (GtkComboBox *combobox_tax_types, gpointer data)
{
    TaxInfoEditor *ti_editor = data;
    gchar* text_selection = NULL;
    GtkTreeIter iter;
    gchar* object_name;
    GtkListStore *store;
    GtkCellRenderer *renderer;
    GtkTreeViewColumn *column;
    GError *error = NULL;
    GtkBuilder *builder;

    /******************************\
     * clear the tax item widgets *
    \******************************/

    clear_tax_item_heading_and_description (ti_editor);
    clear_tax_item_assigned_accounts (ti_editor);
    clear_tax_item_columns (ti_editor);
    clear_tax_item_lines (ti_editor);
    clear_tax_item_forms (ti_editor);

    /*****************************************\
     * clear the elements of the tax cell ID *
    \*****************************************/

    if (ti_editor->tax_type_string)
    {
        g_free (ti_editor->tax_type_string);
        ti_editor->tax_form_string = NULL;
    }
    if (ti_editor->tax_form_string)
    {
        g_free (ti_editor->tax_form_string);
        ti_editor->tax_form_string = NULL;
    }
    if (ti_editor->tax_item_ID_string)
    {
        g_free (ti_editor->tax_item_ID_string);
        ti_editor->tax_item_ID_string = NULL;
    }
    if (ti_editor->tax_cell_ID_string)
    {
        g_free (ti_editor->tax_cell_ID_string);
        ti_editor->tax_cell_ID_string = NULL;
        clear_tax_item_assigned_accounts (ti_editor);
    }

    /*****************************************************\
     * update the forms tree view with the list of forms *
     * belonging to the selected tax type                *
    \*****************************************************/

    /* get the selected tax type */
    gtk_combo_box_get_active_iter (combobox_tax_types, &iter);
    gtk_tree_model_get
        (GTK_TREE_MODEL (gtk_combo_box_get_model(combobox_tax_types)),
         &iter,
         0,
         &ti_editor->tax_type_string,
         -1);

    /* get the list of tax forms that belong to this tax type */
    object_name = g_strdup_printf("list%s", ti_editor->tax_type_string);
    builder = init_builder
                  (object_name,
                   g_strdup_printf("dialog-tax-info-editor-%s-%s.glade",
                   ti_editor->tax_locale_string,
                   ti_editor->tax_type_string));

    if (!builder) return;
    store = GTK_LIST_STORE (gtk_builder_get_object (builder, object_name));
    g_free (object_name);

    if (gtk_combo_box_get_active
           (GTK_COMBO_BOX (ti_editor->combobox_tax_years)) >= 0 )
    {
        /* display the list of tax forms */
        gtk_tree_view_set_model(GTK_TREE_VIEW (ti_editor->tree_view_forms),
                                GTK_TREE_MODEL (store));
        renderer = gtk_cell_renderer_text_new ();
        column = gtk_tree_view_column_new_with_attributes
                     (N_("Form"), /* column heading */
                      renderer, "text",
                      0, /* column */
                      NULL);
        gtk_tree_view_append_column
            (GTK_TREE_VIEW (ti_editor->tree_view_forms), column);
    }

    g_object_unref (builder);

    return;
}

/**************************************************************************\
 * tax_year_changed_cb                                                    *
 * initialize the dialog widgets                                          *
 *                                                                        *
 * Args:   widget pointer - the widget that is sending the signal         *
 *         gpointer data  - pointer to the tax info editor dialog context *
 * Return: nothing                                                        *
\**************************************************************************/

void
tax_year_changed_cb (GtkComboBox *combobox_tax_years, gpointer data)
{
    TaxInfoEditor *ti_editor = data;

    if (gtk_tree_selection_get_selected (ti_editor->form_selection, NULL, NULL))
    {
        tax_form_selection_changed_cb (ti_editor->form_selection, ti_editor);
    }
    else
    {
        if (gtk_combo_box_get_active
               (GTK_COMBO_BOX (ti_editor->combobox_tax_types)) >= 0 )
        {
            tax_type_changed_cb (ti_editor->combobox_tax_types, ti_editor);
        }
    }

    return;
}

/**************************************************************************\
 * tax_country_changed_cb                                                 *
 * initialize the dialog widgets                                          *
 *                                                                        *
 * Args:   widget pointer - the widget that is sending the signal         *
 *         gpointer data  - pointer to the tax info editor dialog context *
 * Return: nothing                                                        *
\**************************************************************************/

void
tax_country_changed_cb (GtkComboBox *combobox_tax_countries, gpointer data)
{
    TaxInfoEditor *ti_editor = data;
    GtkTreeIter iter;
    GtkBuilder *builder;
    gchar* object_name;
    GtkListStore *store;
    GError *error = NULL;

    /***************************************\
     * get the selected tax country locale *
    \***************************************/

    gtk_combo_box_get_active_iter (combobox_tax_countries, &iter);
    gtk_tree_model_get
        (GTK_TREE_MODEL(gtk_combo_box_get_model (combobox_tax_countries)),
         &iter, 1, &(ti_editor->tax_locale_string), -1);

    /***************************\
     * clear the other widgets *
    \***************************/

    clear_tax_item_heading_and_description (ti_editor);
    clear_tax_item_assigned_accounts (ti_editor);
    clear_tax_item_columns (ti_editor);
    clear_tax_item_lines (ti_editor);
    clear_tax_item_forms (ti_editor);

    /*****************************************\
     * clear the elements of the tax cell ID *
    \*****************************************/

    if (ti_editor->tax_form_string)
    {
        g_free (ti_editor->tax_form_string);
        ti_editor->tax_form_string = NULL;
    }
    if (ti_editor->tax_item_ID_string)
    {
        g_free (ti_editor->tax_item_ID_string);
        ti_editor->tax_item_ID_string = NULL;
    }
    if (ti_editor->tax_cell_ID_string)
    {
        g_free (ti_editor->tax_cell_ID_string);
        ti_editor->tax_cell_ID_string = NULL;
        clear_tax_item_assigned_accounts (ti_editor);
    }

    /* unset the model from the comobox for tax types */
    if (gtk_combo_box_get_model (GTK_COMBO_BOX(ti_editor->combobox_tax_types)))
        gtk_combo_box_set_model (GTK_COMBO_BOX(ti_editor->combobox_tax_types),
                                 NULL);

    /* unset the model from the comobox for tax years */
    if (gtk_combo_box_get_model (GTK_COMBO_BOX(ti_editor->combobox_tax_years)))
        gtk_combo_box_set_model (GTK_COMBO_BOX(ti_editor->combobox_tax_years),
                                 NULL);

    /********************************************\
     * fill the combobox of supported tax types *
    \********************************************/

    /* get the list of tax types for this country */
    object_name = g_strdup_printf("listTaxTypes-%s",
                                  ti_editor->tax_locale_string);

    builder = init_builder
                  (object_name,
                   g_strdup_printf("dialog-tax-info-editor-%s-TaxTypes.glade",
                                   ti_editor->tax_locale_string));
    if (!builder) return;

    store = GTK_LIST_STORE (gtk_builder_get_object (builder, object_name));
    g_free (object_name);

    /* set the new model to the comobox */
    gtk_combo_box_set_model (ti_editor->combobox_tax_types,
                             GTK_TREE_MODEL(store));
    g_object_unref (builder);

    /********************************************\
     * fill the combobox of supported tax years *
    \********************************************/

    /* get the list of tax years for this country */
    object_name = g_strdup_printf("listTaxYears-%s",
                                  ti_editor->tax_locale_string);
    builder = init_builder
                  (object_name,
                   g_strdup_printf("dialog-tax-info-editor-%s-TaxYears.glade",
                                   ti_editor->tax_locale_string));
    if (!builder) return;

    store = GTK_LIST_STORE (gtk_builder_get_object (builder, object_name));
    g_free (object_name);

    /* set the new model to the comobox */
    gtk_combo_box_set_model (ti_editor->combobox_tax_years,
                             GTK_TREE_MODEL(store));

    g_object_unref (builder);

    return;
}

/********************************************************************\
 * tax_info_editor_response_handler                                 *
 *   actions for the dialog buttons and window closing              *
 *                                                                  *
 * Args:   dialog - pointer to the dialog                           *
 *         response - response code                                 *
 *         gpointer - link to the ti_editor context structure       *
 * Return: nothing                                                  *
\********************************************************************/

void
tax_info_editor_response_handler (GtkDialog *dialog,
                                  gint response, gpointer user_data)
{
    TaxInfoEditor *ti_editor = user_data;
    const gchar *error_message;

    switch (response)
    {
		case GTK_RESPONSE_OK: gtk_widget_destroy (ti_editor->window);
		                      break;

		case GTK_RESPONSE_HELP:
                                 error_message =
                                     g_strdup_printf ("%s\n%s\n\n%s",
                                         N_("Help is always good"),
                                         N_("if you get it"),
                                         N_("you need to read yourself"));
                                 show_info_dialog (NULL,
                                     GTK_MESSAGE_INFO,
                                     error_message);
                                 break;
	}

    return;
}

/********************************************************************\
 * tax_info_editor_close_handler                                    *
 * currently only called as a consequence of gtk_widget_destroy()   *
 *                                                                  *
 * Args:   gpointer - link to the ti_editor context structure       *
 * Return: nothing                                                  *
\********************************************************************/

void
tax_info_editor_close_handler (GtkDialog *dialog, gpointer user_data)
{
    TaxInfoEditor *ti_editor = user_data;

    g_free (ti_editor);
}

/********************************************************************\
 * gnc_tax_info_editor_create                                       *
 * initialize the dialog widgets                                    *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 *         ti_editor - tax info editor dialog context               *
 * Return: nothing                                                  *
\********************************************************************/

void
gnc_tax_info_editor_create (GtkWidget *parent, TaxInfoEditor *ti_editor)
{
    GtkBuilder *builder;
    GtkMenuItem *menu_item;
    GtkTreeView *tree_view;

    /*********************************\
     * initialize the dialog widgets *
    \*********************************/

    builder = gtk_builder_new();

    /********************************************************************\
     * first read the tree model needed for the tax country combo boxes *
    \********************************************************************/

    gnc_builder_add_from_file (builder,
                               "dialog-tax-info-editor-TaxCountries.glade",
                               "listTaxCountries");

    /********************************************************************\
     * as the last step: read the dialog definition from the glade file *
    \********************************************************************/

    gnc_builder_add_from_file (builder,
                               "dialog-tax-info-editor.glade",
                               "dialog_window");

    /**************************************\
     * update the tax info editor context *
    \**************************************/

    ti_editor->window = GTK_WIDGET (gtk_builder_get_object (builder,
                                                           "dialog_window"));

    /* parent */
    if (parent != NULL)
        gtk_window_set_transient_for (GTK_WINDOW (ti_editor->window),
                                      GTK_WINDOW (parent));

    /* default to ok */
    gtk_dialog_set_default_response (GTK_DIALOG(ti_editor->window),
                                     GTK_RESPONSE_OK);

    ti_editor->combobox_tax_countries =
        GTK_COMBO_BOX (
            gtk_builder_get_object (builder, "combobox_TaxCountries"));

    ti_editor->combobox_tax_types =
        GTK_COMBO_BOX (
            gtk_builder_get_object (builder, "combobox_TaxTypes"));

    ti_editor->combobox_tax_years =
        GTK_COMBO_BOX (
            gtk_builder_get_object (builder, "combobox_TaxYears"));

    /* initialize the combobox for the tax countries */
    gtk_combo_box_set_model
        (ti_editor->combobox_tax_countries,
         GTK_TREE_MODEL(
             GTK_LIST_STORE (
                 gtk_builder_get_object (builder, "listTaxCountries"))));

    ti_editor->tree_view_forms =
        GTK_WIDGET (gtk_builder_get_object (builder, "treeview_TaxForms"));

    ti_editor->form_selection =
        gtk_tree_view_get_selection (
            GTK_TREE_VIEW (ti_editor->tree_view_forms));

    gtk_tree_selection_set_mode (ti_editor->form_selection,
                                 GTK_SELECTION_SINGLE);

    ti_editor->tree_view_lines =
        GTK_WIDGET (gtk_builder_get_object (builder, "treeview_TaxFormLines"));

    ti_editor->line_selection =
        gtk_tree_view_get_selection (
            GTK_TREE_VIEW (ti_editor->tree_view_lines));

    gtk_tree_selection_set_mode (ti_editor->line_selection,
                                 GTK_SELECTION_SINGLE);

    ti_editor->tree_view_columns =
        GTK_WIDGET (gtk_builder_get_object (builder,
                                            "treeview_TaxFormLineColumns"));

    ti_editor->column_selection =
        gtk_tree_view_get_selection (
            GTK_TREE_VIEW (ti_editor->tree_view_columns));

    gtk_tree_selection_set_mode (ti_editor->column_selection,
                                 GTK_SELECTION_SINGLE);

    ti_editor->tax_item_heading =
        GTK_WIDGET (gtk_builder_get_object (builder, "label_ItemHeading"));

    ti_editor->tax_item_description =
        GTK_WIDGET (gtk_builder_get_object (builder, "label_ItemDescription"));

    /* account tree - assigned accounts */

    ti_editor->tree_view_accounts_assigned =
        GTK_WIDGET(gnc_tree_view_account_new (FALSE));

    ti_editor->accounts_assigned_selection =
       gtk_tree_view_get_selection
           (GTK_TREE_VIEW (
               ti_editor->tree_view_accounts_assigned));

    gtk_tree_selection_set_mode (ti_editor->accounts_assigned_selection,
                                 GTK_SELECTION_MULTIPLE);

    gtk_container_add (
        GTK_CONTAINER (
            GTK_WIDGET(gtk_builder_get_object (builder, "scrolledwindow4"))),
        ti_editor->tree_view_accounts_assigned);

    gnc_tree_view_account_set_filter (
        GNC_TREE_VIEW_ACCOUNT(ti_editor->tree_view_accounts_assigned),
        account_filter_hasCurrentTaxID,
        ti_editor,
        NULL);

    /* account tree - account pool */
    ti_editor->tree_view_account_pool =
        GTK_WIDGET(gnc_tree_view_account_new (FALSE));

    ti_editor->account_pool_selection =
        gtk_tree_view_get_selection (
            GTK_TREE_VIEW (ti_editor->tree_view_account_pool));

    gtk_tree_selection_set_mode (ti_editor->account_pool_selection,
                                 GTK_SELECTION_MULTIPLE);

    gtk_container_add (
        GTK_CONTAINER (
            GTK_WIDGET(gtk_builder_get_object (builder, "scrolledwindow5"))),
        ti_editor->tree_view_account_pool);

    gnc_tree_view_account_set_filter (
        GNC_TREE_VIEW_ACCOUNT(ti_editor->tree_view_account_pool),
        account_filter_isNotAssigned,
        ti_editor,
        NULL);

    g_object_unref (builder);

    /* account tree popup menu */
    ti_editor->account_pool_popup_menu = GTK_MENU (gtk_menu_new());
    ti_editor->accounts_assigned_popup_menu = GTK_MENU (gtk_menu_new());

    /* the elements of the tax cell ID */

    ti_editor->tax_locale_string  = NULL;
    ti_editor->tax_type_string    = NULL;
    ti_editor->tax_form_string    = NULL;
    ti_editor->tax_item_ID_string = NULL;
    ti_editor->tax_cell_ID_string = NULL;

    /***********************\
     * connect the signals *
    \***********************/

    g_signal_connect (G_OBJECT (ti_editor->window), "response",
                      G_CALLBACK (tax_info_editor_response_handler),
                      ti_editor);

    g_signal_connect (G_OBJECT (ti_editor->window), "destroy",
                      G_CALLBACK (tax_info_editor_close_handler),
                      ti_editor);

    g_signal_connect (G_OBJECT (ti_editor->combobox_tax_countries),
                      "changed",
                      G_CALLBACK (tax_country_changed_cb), ti_editor);

    g_signal_connect (G_OBJECT (ti_editor->combobox_tax_types),
                      "changed",
                      G_CALLBACK (tax_type_changed_cb), ti_editor);

    g_signal_connect (G_OBJECT (ti_editor->combobox_tax_years),
                      "changed",
                      G_CALLBACK (tax_year_changed_cb), ti_editor);

    g_signal_connect (G_OBJECT (ti_editor->form_selection),
                     "changed",
                      G_CALLBACK (tax_form_selection_changed_cb), ti_editor);

    g_signal_connect (G_OBJECT (ti_editor->line_selection),
                      "changed",
                      G_CALLBACK (tax_line_selection_changed_cb), ti_editor);

    g_signal_connect (G_OBJECT (ti_editor->column_selection),
                      "changed",
                      G_CALLBACK (tax_column_selection_changed_cb), ti_editor);

    g_signal_connect (G_OBJECT (ti_editor->tree_view_account_pool),
                      "row-activated",
                      G_CALLBACK (account_pool_activated_cb), ti_editor);

    g_signal_connect (G_OBJECT (ti_editor->tree_view_accounts_assigned),
                      "row-activated",
                      G_CALLBACK (account_assigned_activated_cb), ti_editor);

    /*************************************************************************\
     * this is not needed  -  but left here as a place holder for future use *
    \*************************************************************************/
    /*
     g_signal_connect (G_OBJECT (ti_editor->tree_view_account_pool),
                       "cursor-changed",
                       G_CALLBACK (account_pool_changed_cb), ti_editor);
    */

    /* connect all that is needed for the account pool pop up menu */

    menu_item = GTK_MENU_ITEM (gtk_menu_item_new_with_label (N_("assign")));

    gtk_menu_shell_append (GTK_MENU_SHELL (ti_editor->account_pool_popup_menu),
                           GTK_WIDGET (menu_item));

    gtk_widget_show (GTK_WIDGET (menu_item));

    g_signal_connect (menu_item,
                      "activate",
                      G_CALLBACK (account_assign_cb), ti_editor);

    menu_item = GTK_MENU_ITEM (gtk_menu_item_new_with_label (N_("look up")));

    gtk_menu_shell_append (GTK_MENU_SHELL (ti_editor->account_pool_popup_menu),
                           GTK_WIDGET (menu_item));

    gtk_widget_show (GTK_WIDGET (menu_item));

    g_signal_connect (menu_item,
                      "activate",
                      G_CALLBACK (account_lookup_cb), ti_editor);

    /* connect all that is needed for the accounts assigned pop up menu */

    menu_item = GTK_MENU_ITEM (gtk_menu_item_new_with_label (N_("unassign")));

    gtk_menu_shell_append (
        GTK_MENU_SHELL (ti_editor->accounts_assigned_popup_menu),
        GTK_WIDGET (menu_item));

    gtk_widget_show (GTK_WIDGET (menu_item));

    g_signal_connect (menu_item,
                      "activate",
                      G_CALLBACK (account_unassign_cb), ti_editor);

    /* connect the handler which will popup the menus */

    g_signal_connect_swapped (GTK_WIDGET(ti_editor->tree_view_account_pool),
                              "button_press_event",
                              G_CALLBACK (account_tree_popup_handler),
                              ti_editor->account_pool_popup_menu);

    g_signal_connect_swapped (
        GTK_WIDGET(ti_editor->tree_view_accounts_assigned),
        "button_press_event",
        G_CALLBACK (account_tree_popup_handler),
        ti_editor->accounts_assigned_popup_menu);

    /********************************************************************\
     * Initialize the country combo box with the first available entry. *
    \********************************************************************/

    gtk_combo_box_set_active(ti_editor->combobox_tax_countries, 0);

    return;
}

/********************************************************************\
 * gnc_tax_info_editor                                              *
 *   opens up a window to set account tax information               *
 *   adjusted for tax declarations in selectable countries          *
 *                                                                  *
 * Args:   parent  - the parent of the window to be created         *
 * Return: nothing                                                  *
\********************************************************************/

void
gnc_tax_info_editor (GtkWidget *parent)
{
    TaxInfoEditor *ti_editor;
    gint component_id;

    ti_editor = g_new0 (TaxInfoEditor, 1);

    gnc_tax_info_editor_create (parent, ti_editor);

    gtk_widget_show_all (ti_editor->window);

    return;
}
